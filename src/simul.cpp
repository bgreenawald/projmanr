#include <Rcpp.h>
#include <string>
#include <cstring>
#include <cmath>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

class Task {

  public:

  // Fields
  /*
   * est: early start
   * ef: early finish
   * lst: late start
   * lf: late finish
   * ci: critical index
   * id: unique identifier
   * name: name of task
   * pred_id: ids of predecessors to a given task
   * succ_id: ids of successors to given task
   */
  double duration, est, ef, lst, lf, ci;
  std::string id, name;
  std::vector<std::string> pred_id, succ_id;

  // Constructor
  Task(string, string, double, string);


  // Getters
  double get_duration(){return duration;}
  vector<string> get_pred(){return pred_id;}
  vector<string> get_succ(){return succ_id;}
  string get_id(){return id;}
  string get_name(){return name;}
  double get_est(){return est;}

  // Setters
  void set_succ(vector<string> new_succ){succ_id = new_succ;}
  void set_est(double new_est){est = new_est;}
  void set_ef(double new_ef){ef = new_ef;}
  void set_lst(double new_lst){lst = new_lst;}
  void set_lf(double new_lf){lf = new_lf;}

  // Member functions
  void reset(){est = ef = lst = lf = 0;}
  string toString();
};

// Function declarations
vector<string> split(string str, char delimiter);
string trim(const string& str);
void get_successors(map<string, Task*> tasks);
string print_vec(vector<string> vect);
void walk_ahead(map<string, Task*> all_tasks, vector<string> ordered_ids);
void walk_back(map<string, Task*> all_tasks, vector<string> ordered_ids);
double crit_path(map<string, Task*> all_tasks);
Rcpp::List critical_path(map<string, Task*> all_tasks, vector<string> ordered_ids,
                             int num, map<string, Rcpp::DoubleVector> dists);

// Constructor
Task::Task(string ids, string names, double dur, string pred){
  duration = dur;
  id = ids;
  name = names;
  pred_id = split(pred, ',');
  est = ef = lst = lf = ci = 0;
}

string Task::toString(){
  std::ostringstream strs;
  strs << est;
  std::string est_str = strs.str();

  std::ostringstream strs1;
  strs1 << ef;
  std::string ef_str = strs1.str();

  std::ostringstream strs2;
  strs2 << lst;
  std::string lst_str = strs2.str();

  std::ostringstream strs3;
  strs3 << lf;
  std::string lf_str = strs3.str();

  return id + ": " + "\n" + "preds: " + print_vec(pred_id) + "\nsucc: " + print_vec(succ_id) +
    "\nest: " + est_str + "\nef: " + ef_str + "\nlst: " + lst_str +
    "\nlf: " + lf_str + "\n\n";
}

// [[Rcpp::export]]
Rcpp::List simul(Rcpp::DataFrame df, Rcpp::CharacterVector ids, int nums, Rcpp::List ls){

  // Read in the elements of the data
  Rcpp::CharacterVector task_ids = df["id"];
  Rcpp::CharacterVector names = df["name"];
  Rcpp::DoubleVector durations = df["duration"];
  Rcpp::CharacterVector preds = df["preds"];
  Rcpp::Rcout << "Initializing objects....." << endl;

  // Create map of tasks and random values
  map<string, Rcpp::DoubleVector> dists;
  map<string, Task*> all_tasks;
  Rcpp::CharacterVector dist_names = ls.names();
  vector<string> ordered_id;

  for(int i = 0; i < dist_names.size(); i++){
    string name = Rcpp::as<string>(dist_names[i]);
    dists.insert(pair<string, Rcpp::DoubleVector>(name, ls[name]));
  }

  for(int i = 0; i < ids.size(); i++){
    string temp_id = Rcpp::as<string>(task_ids[i]);
    Task* temp = new Task(temp_id, Rcpp::as<string>(names[i]), durations[i], Rcpp::as<string>(preds[i]));
    all_tasks.insert(pair<string, Task*>(temp_id, temp));
    ordered_id.push_back(Rcpp::as<string>(ids[i]));
  }

  get_successors(all_tasks);
  return critical_path(all_tasks, ordered_id, nums, dists);

}

// String split
vector<string> split(string str, char delimiter) {
  vector<string> internal;
  stringstream ss(str); // Turn the string into a stream.
  string tok;

  while(getline(ss, tok, delimiter)) {
    internal.push_back(trim(tok));
  }

  return internal;
}

// String trim whitespace
string trim(const string& str) {
  size_t first = str.find_first_not_of(' ');
  if (string::npos == first)
  {
    return str;
  }
  size_t last = str.find_last_not_of(' ');
  return str.substr(first, (last - first + 1));
}

// Get the successors for each task
void get_successors(map<string, Task*> tasks){
  // Iterate over each task
  for(map<string, Task*>::iterator iter = tasks.begin(); iter != tasks.end(); ++iter){
    Task* current_task = iter->second;
    vector<string> temp_succ;

    // Iterate over each task again, comparing the current task to each list of predeccessors
    for(map<string, Task*>::iterator iter2 = tasks.begin(); iter2 != tasks.end(); ++iter2){
      Task* temp = iter2->second;
      vector<string> pred_ids = temp->pred_id;
      if(std::find(pred_ids.begin(), pred_ids.end(), current_task->get_id()) != pred_ids.end()){
        temp_succ.push_back(temp->id);
      }
    }
    current_task->set_succ(temp_succ);
  }
}

// Print a string vector
string print_vec(vector<string> vect){
  string ret = "";
  for(unsigned int i = 0; i < vect.size(); i++){
    ret += vect[i] + " ";
  }

  return ret;
}

// Perform the walk ahead step of the critical path
void walk_ahead(map<string, Task*> all_tasks, vector<string> ordered_ids){
  for(vector<string>::iterator it = ordered_ids.begin() ; it != ordered_ids.end(); ++it){
    Task* current_task = all_tasks[*it];
    vector<string> preds = current_task->pred_id;
    if(preds.size() != 0){
      for(vector<string>::iterator it2 = preds.begin() ; it2 != preds.end(); ++it2){
        if(std::find(preds.begin(), preds.end(), *it2) == preds.end()){
          throw "Invalid predeccessor id. Using a predeccessor id for a task that does not exist.";
        }
        Task* pred_task = all_tasks[*it2];
        if(current_task->est <= pred_task->ef){
          current_task->est = pred_task->ef;
        }
      }
    }
    current_task->ef = current_task->est + current_task->duration;
  }
}

// Perform the walk back of the critical path
void walk_back(map<string, Task*> all_tasks, vector<string> ordered_ids){
  for(vector<string>::reverse_iterator it = ordered_ids.rbegin() ; it != ordered_ids.rend(); ++it){
    Task* current_task = all_tasks[*it];
    vector<string> succ = current_task->succ_id;
    if(succ.size() != 0){
      for(vector<string>::iterator it2 = succ.begin() ; it2 != succ.end(); ++it2){
        if(std::find(succ.begin(), succ.end(), *it2) == succ.end()){
          throw "Invalid predeccessor id. Using a predeccessor id for a task that does not exist.";
        }
        Task* succ_task = all_tasks[*it2];
        if(current_task->lf == 0 || current_task->lf > succ_task->lst){
          current_task->lf = succ_task->lst;
        }
      }
    }else{
      current_task->lf = current_task->ef;
    }
    current_task->lst = current_task->lf - current_task->duration;
  }
}

// Find the actual critical path and gets total duration
double crit_path(map<string, Task*> tasks){
  double total_duration = 0;
  // Iterate over every task and see if it is in the critical path
  // Also use this to update the critical index for a given task
  for(map<string, Task*>::iterator iter = tasks.begin(); iter != tasks.end(); ++iter){
    Task* cur_task = iter->second;
    // Check to see if the current task is critical
    if(abs(cur_task->ef - cur_task->lf) < 0.00001 && abs(cur_task->est - cur_task->lst) < 0.00001){
      // Update the duration
      total_duration += cur_task->duration;
      // Update the critical index
      cur_task->ci++;
    }
  }
  return total_duration;
}

// Wrapper function that performs the entire critical path computation
Rcpp::List critical_path(map<string, Task*> tasks, vector<string> ordered_ids,
                             int num, map<string, Rcpp::DoubleVector> dists){
  // Take care of the source and sink node
  Task* source = new Task("%id_source%", "%id_source%", 0, "");
  Task* sink = new Task("%id_sink%", "%id_sink%", 0, "");
  vector<string> source_succ;
  vector<string> sink_pred;
  for(map<string, Task*>::iterator iter = tasks.begin(); iter != tasks.end(); ++iter){
    Task* cur = iter->second;
    if((cur->pred_id).size() == 0){
      (cur->pred_id).push_back("%id_source%");
      source_succ.push_back(cur->id);
    }

    if((cur->succ_id).size() == 0){
      (cur->succ_id).push_back("%id_sink%");
      sink_pred.push_back(cur->id);
    }
  }

  source->succ_id = source_succ;
  sink->pred_id = sink_pred;
  tasks.insert(pair<string, Task*>(source->id, source));
  tasks.insert(pair<string, Task*>(sink->id, sink));
  ordered_ids.insert(ordered_ids.begin(), "%id_source%");
  ordered_ids.push_back("%id_sink%");

  Rcpp::Rcout << "Running Simulation....." << endl;
  vector<double> ret(num);
  for(int i = 0; i < num; i++){

    Rcpp::checkUserInterrupt();

    // Reset all tasks
    for(map<string, Task*>::iterator iter = tasks.begin(); iter != tasks.end(); ++iter){
      iter->second->reset();
    }

    // Get the new duration for certain tasks
    for(map<string, Rcpp::DoubleVector>::iterator iter = dists.begin(); iter != dists.end(); ++iter){
      tasks[iter->first]->duration = dists[iter->first][i];
    }


    walk_ahead(tasks, ordered_ids);
    walk_back(tasks, ordered_ids);
    ret[i] = crit_path(tasks);
  }
  
  // Calculate critical index and append results return value
  // Create vectors of the task ids and critical indexes
  Rcpp::CharacterVector ci_ids;
  Rcpp::DoubleVector cis;
  // Iterate over every task
  for(map<string, Task*>::iterator iter =tasks.begin(); iter != tasks.end(); ++iter){
    Task* cur_task = iter->second;
    
    // Check to make sure the task isn't the source or sink
    if(cur_task->get_id() != "%id_sink%" && cur_task->get_id() != "%id_source%"){
      // Push the current task id onto id list
      ci_ids.push_back(cur_task->get_id());
      //Push the current task C.I onto C.I list
      // AFTER dividing by total number of tasks
      cis.push_back(cur_task->ci/num);
    }
    
  }
  
  // Create a dataframe mapping the task indexes to the 
  // corresponding critical index
  Rcpp::DataFrame ret_cis = Rcpp::DataFrame::create( 
                                        Rcpp::Named("ids")= ci_ids, 
                                        Rcpp::Named("critical_indexes") = cis);
  
  // Construct a return list containing the distributions and 
  // the critical indexes
  return Rcpp::List::create(Rcpp::Named("distributions") = ret,
                            Rcpp::Named("critical_indexes") = ret_cis);
}
