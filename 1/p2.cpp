#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>

using namespace std;

int main() {
    int sim = 0;
    vector<int> l1;
    vector<int> l2;

    ifstream file("input.txt");
    if (!file) {
        cerr << "Error: Cannot open file.\n";
        return 1;
    }

    string line;
    while (getline(file, line)) {
        istringstream iss(line);
        int value1, value2;
        iss >> value1 >> value2;

        l1.push_back(value1);
        l2.push_back(value2);
    }

    file.close();

    sort(l1.begin(), l1.end());
    sort(l2.begin(), l2.end());

    for (int val : l1) {
        sim += count(l2.begin(), l2.end(), val) * val;
    }

    cout << sim << endl;

    return 0;
}

