#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

using namespace std;

int main() {
    int safe = 0;
    ifstream file("input.txt");

    if (!file) {
        cerr << "Error: Cannot open file.\n";
        return 1;
    }

    std::string line;
    while (getline(file, line)) {
        vector<int> tab;
        stringstream ss(line);
        int value;

        while (ss >> value) {
            tab.push_back(value);
        }

        if (tab.size() < 2) {
            continue;
        }

        bool dec = true;
        bool inc = true;

        for (size_t i = 1; i < tab.size(); ++i) {
            int diff = tab[i] - tab[i - 1];

            if (diff > 0 && diff <= 3) {
                dec = false;
            } else if (diff < 0 && diff >= -3) {
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

        if (inc || dec) {
            ++safe;
        }
    }

    cout << safe << endl;
    return 0;
}

