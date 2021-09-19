#include <iostream>
#include <vector>
#include <unordered_map>
#include <algorithm>

using namespace std;

unordered_multimap<int, vector<int>> snowflakes;
vector<vector<int>> matches;

void set_matches(pair<const int, vector<int>> match) {
  matches.push_back(get<1>(match));
}

int get_hash(const vector<int>& sf, const int n) {
  int sum = 0;
  for (int i : sf) {
    sum += i;
  }
  return sum % n;
}

int ident_right(const vector<int>& snow1, const vector<int>& snow2, int start) {
  for (int i=0; i!=6; i++) {
    int offset = (start + i) % 6;
    if (snow1[i] != snow2[offset]) {
      return 0;
    }
  }
  return 1;
}

int ident_left(const vector<int>& snow1, const vector<int>& snow2, int start) {
  for (int i=0; i!=6; i++) {
    int offset = start - i;
    if (offset < 0) offset += 6;
    if (snow1[i] != snow2[offset]) {
      return 0;
    }
  }
  return 1;
}

int are_ident(const vector<int>& sf1, const vector<int>& sf2) {
  for (int i=0; i!=6; i++) {
    if (ident_right(sf1, sf2, i)) {
      return 1;
    }
    if (ident_left(sf1, sf2, i)) {
      return 1;
    }
  }
  return 0;
}

int find_ident(const vector<int>& sf, const vector<vector<int>>& matches) {
  for (size_t i=0; i!=matches.size(); i++) {
    if (are_ident(sf, matches[i])) {
      return 1;
    }
  }
  return 0;
}

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  int n;
  cin >> n;
  vector<int> sf(6);
  int hashcode;

  for (int i=0; i<n; i++) {
    for (int j=0; j<6; j++) {
      cin >> sf[j];
    }
    hashcode = get_hash(sf, n);
    if (snowflakes.count(hashcode) != 0) {
      matches.clear();
      auto range = snowflakes.equal_range(hashcode);
      for_each (range.first, range.second, set_matches);
      if (find_ident(sf, matches)) {
        cout << "Twin snowflakes found.\n";
        return 0;
      } else {
        snowflakes.emplace(hashcode, sf);
      }
    } else {
      snowflakes.emplace(hashcode, sf);
    }
  }

  cout << "No two snowflakes are alike.\n";
}
