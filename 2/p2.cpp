#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

bool checkSafe(const vector<int>& tab) {
    bool dec = true;
    bool inc = true;

    for (size_t i = 1; i < tab.size(); ++i) {
        int diff = tab[i] - tab[i - 1];

        if (diff > 0 && diff <= 3 && inc) {
            dec = false;
        } else if (diff < 0 && diff >= -3 && dec) {
            inc = false;
        } else {
            inc = false;
            dec = false;
            break;
        }

        if (!inc && !dec) {
            break;
        }
    }

    return inc || dec;
}

int main() {
    int safe = 0;
    ifstream file("input.txt");

    if (!file) {
        cerr << "Error: Cannot open file.\n";
        return 1;
    }

    string line;
    while (getline(file, line)) {
        vector<int> tab;
        stringstream ss(line);
        int value;

        while (ss >> value) {
            tab.push_back(value);
        }

        if (checkSafe(tab)) {
            ++safe;
        } else {
            for (size_t i = 0; i < tab.size(); ++i) {
                vector<int> subTab(tab.begin(), tab.end());
                subTab.erase(subTab.begin() + i);

                if (checkSafe(subTab)) {
                    ++safe;
                    break;
                }
            }
        }
    }

    cout << safe << endl;
    return 0;
}

