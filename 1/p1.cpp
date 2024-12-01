#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <algorithm>
#include <cmath>

using namespace std;

int main() {
    long long acc = 0;
    vector<int> l1, l2;

    ifstream file("input.txt");
    if (!file) {
        cerr << "Error: Cannot open file.\n";
        return 1;
    }

    string line;
    while (getline(file, line)) {
        istringstream iss(line);
        int first, second;
        iss >> first >> second;
        l1.push_back(first);
        l2.push_back(second);
    }

    file.close();

    sort(l1.begin(), l1.end());
    sort(l2.begin(), l2.end());

    for (size_t i = 0; i < l1.size(); ++i) {
        acc += abs(l1[i] - l2[i]);
    }

    cout << acc << endl;

    return 0;
}

