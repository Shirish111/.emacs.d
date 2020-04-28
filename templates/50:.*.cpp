#include <bits/stdc++.h>
using namespace std;

template<typename T>
inline void midch(T &mid, T l, T h) {
  mid = l + ((h - l) >> 1);
}

template <typename T>
inline void maxch(T &x, T y) {
  x = x > y ? x : y;
}

template <typename T>
inline void minch(T &x, T y) {
  x = x < y ? x : y;
}

template <typename T, typename U>
ostream &operator<<(ostream &os, pair<T, U> &p) {
  os << p.first << " " << p.second;
  return os;
}

template <typename T>
ostream &operator<<(ostream &os, vector<T> &v) {
  for (auto &i : v) {
    os << i << " ";
  }
  return os;
}

template <typename T>
ostream &operator<<(ostream &os, vector<vector<T>> &v) {
  for (auto &i : v) {
    os << i << "\n";
  }
  return os;
}

template <typename T, typename U>
ostream &operator<<(ostream &os, unordered_map<T, U> &m) {
  for (auto &i : m) {
    os << i.first << " " << i.second << " ";
  }
  return os;
}

template <typename T, typename U>
ostream &operator<<(ostream &os, map<T, U> &m) {
  for (auto &i : m) {
    os << i.first << " " << i.second << " ";
  }
  return os;
}

//--------------------------

$0

void solve() {

}
int main() {
  cin.tie(NULL);
  ios_base::sync_with_stdio(false);
  solve();
  return 0;
}
