#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>

#include "kemenydll.h"

using namespace std;

__stdcall const char* getName(){
    std::string str = "kemeny";
    return str.c_str();
}