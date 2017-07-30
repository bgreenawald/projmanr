#include <Rcpp.h>
#include <string>
#include <list>
#include <cstring>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

// String split 
vector<string> split(string str, char delimiter) {
  vector<string> internal;
  stringstream ss(str); // Turn the string into a stream.
  string tok;
  
  while(getline(ss, tok, delimiter)) {
    internal.push_back(tok);
  }
  
  return internal;
}


// Task class
class Task {
  int duration, est, ef, lst, lf;
  std::string id, name;
  std::vector<std::string> pred_id, succ_id;
public:
  Task(int, string, string, string);
  int get_duration(){return duration;}
  void get_vec();
};

Task::Task(int dur, string id, string name, string pred){
	duration = dur;
	id = id;
	name = name;
	pred_id = split(pred, ',');
}

void Task::get_vec(){
	for(int i = 0; i < pred_id.size(); i++){
		cout << pred_id[i] << endl;
	}
}

// [[Rcpp::export]]
int main(){
	list<Task> tasks;
	Task* t = new Task(4, "1", "t1", "1,2");
	(*t).get_vec();
	tasks.push_back(*t);
	
	return 0;
}