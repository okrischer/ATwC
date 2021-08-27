#include <iostream>
#include <vector>

using namespace std;

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

int are_ident(const vector<int>& snow1, const vector<int>& snow2) {
  for (int i=0; i!=6; i++) {
    if (ident_right(snow1, snow2, i)) {
      return 1;
    }
    if (ident_left(snow1, snow2, i)) {
      return 1;
    }
  }
  return 0;
}

int find_ident(const vector<vector<int>>& snowflakes) {
  for (size_t i=0; i!=snowflakes.size(); i++) {
    for (size_t j=i+1; j!=snowflakes.size(); j++) {
      if (are_ident(snowflakes[i], snowflakes[j])) {
        return 1;
      }
    }
  }
  return 0;
}

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  int n;
  cin >> n;
  vector<vector<int>> snowflakes(n);
  vector<int> sf(6);

  for (int i=0; i<n; i++) {
    for (int j=0; j<6; j++) {
      cin >> sf[j];
    }
    snowflakes[i] = sf;
  }
  if (find_ident(snowflakes)) {
    cout << "Twin snowflakes found.";
  } else {
    cout << "No two snowflakes are alike.";
  }
  cout << endl;
}
