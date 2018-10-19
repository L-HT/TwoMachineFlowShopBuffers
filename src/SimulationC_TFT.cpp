#include <Rcpp.h>
#include <vector>
#include <cassert>
#include <sstream>

#include "HelperFunctions.h"
#include "SimulationC.h"
#include "Machine.cpp"


int simulateFlowShopC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      std::vector<std::string> permutationM2, int maxBufferSize,
                      bool abortOnInvalidity){
  Machine m1;
  Machine m2;
  m1.name_ = "m1";
  m2.name_ = "m2";
  std::size_t nextJobM1 = 0;
  std::size_t nextJobM2 = 0;
  StatusReport m1Event("", "");
  StatusReport m2Event("", "");
  int time = 0;
  bool finished = false;
  bool m2AllowedToWork = false;

  int result = 0;
  Rcpp::StringVector jobNames = jobDataDF["job"];
  Rcpp::IntegerVector m1Times = jobDataDF["m1Time"];
  Rcpp::IntegerVector m2Times = jobDataDF["m2Time"];
  Rcpp::IntegerVector bufferUsage = jobDataDF["bufferUsage"];

  std::vector<Job> jobData;
  jobData.reserve(m1Times.size());
  for (int i = 0; i < m1Times.size(); i++){
    Job job((std::string) jobNames(i), (int) m1Times(i), (int) m2Times(i), (int) bufferUsage(i));
    jobData.push_back(job);
  }

  int totalM2Time = 0;
  for (int i = 0; i < m1Times.size(); i++){
    totalM2Time += m1Times(i) + m2Times(i);
  }

  int m2Wait = 0;

  std::vector<Job> jobsJustFinished;
  std::vector<Job> buffer;

  while(!finished){
    // Rcpp::Rcout << time << std::endl;
    if (!m2.busy_){
      //Suche nach passendem Job im Buffer
      int positionOfNextJob = -1;
      for (std::size_t i = 0; i < buffer.size(); i++){
        if (buffer[i].name_ == permutationM2[nextJobM2]){
          positionOfNextJob = i;
        }
      }

      if (positionOfNextJob != -1){
        buffer.erase(buffer.begin() + positionOfNextJob);
        m2AllowedToWork = true;
      }
    }
    // Rcpp::Rcout << "Busy check" << std::endl;
    if (!jobsJustFinished.empty()){
      if (permutationM2[nextJobM2] == jobsJustFinished[0].name_ && !m2.busy_){
        m1.busy_ = false;

        assert(m2AllowedToWork == false);
        m2AllowedToWork = true;
        if (m1.blocked_){
          m1.blocked_ = false;
        }
        jobsJustFinished.erase(jobsJustFinished.begin());
      } else {

        int currentBufferUsage = 0;
        for (std::vector<Job>::const_iterator it = buffer.begin(); it != buffer.end(); it++){
          currentBufferUsage += (*it).bufferUsage_;
        }

        int justFinishedJobsBufferUsage = 0;
        for (std::vector<Job>::const_iterator it = jobsJustFinished.begin(); it != jobsJustFinished.end(); it++){
          justFinishedJobsBufferUsage += (*it).bufferUsage_;
        }
        if (currentBufferUsage + justFinishedJobsBufferUsage <= maxBufferSize){
          buffer.insert(buffer.end(), jobsJustFinished.begin(), jobsJustFinished.end());
          m1.busy_ = false;
          if (m1.blocked_){
            m1.blocked_ = false;

          }
          jobsJustFinished.clear();
        } else {
          if (!m1.blocked_){
            m1.blocked_ = true;
            m1.busy_ = true;
            m1Event.state_ = "blocked";
          }
        }
      }
    }
    // Rcpp::Rcout << "Jobs just finished check" << std::endl;
    if (!m1.turnedOff_){
      if (!m1.busy_){
        Job nextJob("", 0, 0, 0);
        for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
          if (permutationM1[nextJobM1] == (*it).name_){
            nextJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
          }
        }

        m1Event = m1.give(nextJob.name_, nextJob.m1Time_);
      }
      if (!m1.blocked_){
        m1Event = m1.work();
      }
    }
    // Rcpp::Rcout << "M1 macht " << m1.currentJob_ << std::endl;
    if (!m2.turnedOff_){
      if (!m2.busy_){
        if (m2AllowedToWork){
          Job nextJob("", 0, 0, 0);
          for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
            if (permutationM2[nextJobM2] == (*it).name_){
              nextJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
            }
          }
          m2Event = m2.give(nextJob.name_, nextJob.m2Time_);
          m2Event = m2.work();
        } else {
          m2Wait++;
          m2Event.state_ = "waiting";
        }
      } else {
        m2Event = m2.work();
      }
    }
    // Rcpp::Rcout << "M2 macht" << m2.currentJob_ << std::endl;


    if (m1Event.state_ == "end"){
      nextJobM1++;
      if (nextJobM1 >= permutationM1.size()){
        m1.turnedOff_ = true;
        m1Event.state_ = "turnedOff";
      }
      // den gerade fertiggestellten Job suchen
      Job tempJob("", 0, 0, 0);
      for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
        if (m1Event.jobName_ == (*it).name_){
          tempJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
        }
      }
      jobsJustFinished.push_back(tempJob);
      // Rcpp::Rcout << "nextJobM1: " << nextJobM1 << std::endl;
    }
    // Rcpp::Rcout << "M1 end:  " << m1Event.jobName_ << " " << m1Event.state_ << std::endl;
    if (m2Event.state_ == "end"){

      m2.busy_ = false;
      m2AllowedToWork = false;
      nextJobM2++;
      m2Wait = 0;

      result += time + 1;
      if (nextJobM2 >= permutationM2.size()){
        finished = true;
        m2.turnedOff_ = true;
        m2Event.state_ = "turnedOff";
      }
      // Rcpp::Rcout << "nextJobM2: " << nextJobM2 << std::endl;
    }
    // Rcpp::Rcout << "M2 end " << m2Event.jobName_ << " " << m2Event.state_ << std::endl;
    if (m2Wait > totalM2Time){
      if (abortOnInvalidity){
        printVector(permutationM1);
        printVector(permutationM2);
        Rcpp::Rcout << "nextJobM2: " << permutationM2[nextJobM2] << std::endl;
        Rcpp::stop("M2 wartet (anscheinend ewig) auf einen Job?");
      } else {
        return -100;
      }
    }

    if (m2Event.state_ == "error" || m1Event.state_ == "error"){
      printVector(permutationM1);
      printVector(permutationM2);
      Rcpp::stop("Error");
    }
    time++;
  }

  return result;
}

int simulateFlowShopTotalBufferC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      std::vector<std::string> permutationM2, int maxBufferSize,
                      bool abortOnInvalidity){
  Machine m1;
  Machine m2;
  m1.name_ = "m1";
  m2.name_ = "m2";
  std::size_t nextJobM1 = 0;
  std::size_t nextJobM2 = 0;
  StatusReport m1Event("", "");
  StatusReport m2Event("", "");
  int time = 0;
  int result = 0;
  bool finished = false;
  //bool m2AllowedToWork = false;

  Rcpp::StringVector jobNames = jobDataDF["job"];
  Rcpp::IntegerVector m1Times = jobDataDF["m1Time"];
  Rcpp::IntegerVector m2Times = jobDataDF["m2Time"];
  Rcpp::IntegerVector bufferUsage = jobDataDF["bufferUsage"];

  std::vector<Job> jobData;
  jobData.reserve(m1Times.size());
  for (int i = 0; i < m1Times.size(); i++){
    Job job((std::string) jobNames(i), (int) m1Times(i), (int) m2Times(i), (int) bufferUsage(i));
    jobData.push_back(job);
  }

  int totalTime = 0;
  for (int i = 0; i < m1Times.size(); i++){
    totalTime += m1Times(i) + m2Times(i);
  }


  int m1Wait = 0;
  int m2Wait = 0;

  std::vector<Job> jobsJustFinished;
  std::vector<Job> buffer;

  while(!finished){

    if (!m1.turnedOff_){
      if (!m1.busy_){
        Job nextJob("", 0, 0, 0);
        for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
          if (permutationM1[nextJobM1] == (*it).name_){
            nextJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
          }
        }

        int currentBufferUsage = 0;
        for (std::vector<Job>::const_iterator it = buffer.begin(); it != buffer.end(); it++){
          currentBufferUsage += (*it).bufferUsage_;
        }

        if (currentBufferUsage + nextJob.bufferUsage_ <= maxBufferSize){
          m1Event = m1.give(nextJob.name_, nextJob.m1Time_);
          buffer.push_back(nextJob);
          m1Event = m1.work();
        } else {
          m1Wait++;
          m1Event.state_ = "waiting";
        }
      } else {
        m1Event = m1.work();
      }
    }

    if (!m2.turnedOff_){
      if (!m2.busy_){

        Job nextJob("", 0, 0, 0);
        for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
          if (permutationM2[nextJobM2] == (*it).name_){
            nextJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
          }
        }

        // Job suchen
        int positionOfNextJob = -1;
        for (std::size_t i = 0; i < jobsJustFinished.size(); i++){
          if (jobsJustFinished[i].name_ == permutationM2[nextJobM2]){
            if (positionOfNextJob != -1){
              std::stringstream ss;
              ss << "Job " << nextJob.name_ << " wurde auf M1 zwei Mal abgeschlossen!?";
              Rcpp::stop(ss.str());
            }
            positionOfNextJob = i;

          }
        }

        // if (positionOfNextJob != -1){
        //   buffer.erase(buffer.begin() + positionOfNextJob);
        //   m2AllowedToWork = true;
        // }

        if (positionOfNextJob != -1){
          m2Event = m2.give(nextJob.name_, nextJob.m2Time_);
          m2Event = m2.work();
          jobsJustFinished.erase(jobsJustFinished.begin() + positionOfNextJob);
        } else {
          m2Wait++;
          m2Event.state_ = "waiting";
        }
      } else {
        m2Event = m2.work();
      }
    }

    if (m1Event.state_ == "end"){
      nextJobM1++;
      m1.busy_ = false;
      if (nextJobM1 >= permutationM1.size()){
        m1.turnedOff_ = true;
        m1Event.state_ = "turnedOff";
      }
      // den gerade fertiggestellten Job suchen
      Job tempJob("", 0, 0, 0);
      for (std::vector<Job>::const_iterator it = jobData.begin(); it != jobData.end(); it++){
        if (m1Event.jobName_ == (*it).name_){
          tempJob.set((*it).name_, (*it).m1Time_, (*it).m2Time_, (*it).bufferUsage_);
        }
      }
      jobsJustFinished.push_back(tempJob);
    }

    if (m2Event.state_ == "end"){
      // den gerade fertiggestellten Job suchen
      std::size_t jobToRemove = 0;
      for (std::size_t i = 0; i < buffer.size(); i++){
        if (buffer[i].name_ == m2Event.jobName_){
          jobToRemove = i;
        }
      }
      buffer.erase(buffer.begin() + jobToRemove);

      m2.busy_ = false;
      //m2AllowedToWork = false;
      nextJobM2++;
      m2Wait = 0;

      result += time + 1;

      if (nextJobM2 >= permutationM2.size()){
        finished = true;
        m2.turnedOff_ = true;
        m2Event.state_ = "turnedOff";
      }
    }

    if (m1Wait > totalTime){
      if (abortOnInvalidity){
        printVector(permutationM1);
        printVector(permutationM2);
        Rcpp::stop("M1 wartet (anscheinend ewig) auf einen Job?");
      } else {
        return -100;
      }
    }
    if (m2Wait > totalTime){
      if (abortOnInvalidity){
        printVector(permutationM1);
        printVector(permutationM2);
        Rcpp::stop("M2 wartet (anscheinend ewig) auf einen Job?");
      } else {
        return -100;
      }
      // Rcpp::stop("M2 wartet (anscheinend ewig) auf einen Job?");
    }

    if (m2Event.state_ == "error" || m1Event.state_ == "error"){
      printVector(permutationM1);
      printVector(permutationM2);
      Rcpp::stop("Error");
    }
    time++;
  }
  return result;
}


