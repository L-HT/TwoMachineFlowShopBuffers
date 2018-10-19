#include <Rcpp.h>
#include <vector>
#include <sstream>


struct Job{
  std::string name_;
  int m1Time_;
  int m2Time_;
  int bufferUsage_;

  Job(std::string name, int m1Time, int m2Time, int bufferUsage){
    set(name, m1Time, m2Time, bufferUsage);
  }
  void set(std::string name, int m1Time, int m2Time, int bufferUsage){
    name_ = name;
    m1Time_ = m1Time;
    m2Time_ = m2Time;
    bufferUsage_ = bufferUsage;
  }
};

struct StatusReport{
  std::string jobName_;
  std::string state_;

  StatusReport(std::string jobName, std::string state)
    : jobName_(jobName), state_(state){

  }
};

struct Machine{
  std::string name_;
  std::string currentJob_;
  int jobTime_;
  int executedTime_;
  bool busy_ = false;
  bool blocked_ = false;
  bool turnedOff_ = false;

  StatusReport work(){
    if (!blocked_){
      executedTime_++;
      if (executedTime_ == jobTime_){
        return StatusReport(currentJob_, "end");
      }
      if (executedTime_ > jobTime_){
        std::stringstream ss;
        ss << "Machine.cpp: " << name_ << ": bearbeitet den Job >>" << currentJob_ << "<< zu viel." << std::endl;
        ss << "Soll: " << jobTime_ << ", Ist: " << executedTime_;
        Rcpp::Rcerr << ss.str() << std::endl;
        return StatusReport(currentJob_, "error");
        //Rcpp::stop(ss.str());
      }
      return StatusReport(currentJob_, "none");
    }
    return StatusReport(currentJob_, "none");
  }

  StatusReport give(std::string jobName, int time){
    currentJob_ = jobName;
    jobTime_ = time;
    executedTime_ = 0;
    busy_ = true;
    blocked_ = false;
    return StatusReport(currentJob_, "start");
  }
};


