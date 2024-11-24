
#pragma once

#include "anyfin/base.hpp"
#include "anyfin/allocator.hpp"

namespace Fin {

template <typename T>
struct List {
  using Value_Type = T;
  
  struct Node {
    Value_Type value;
    Node *next = nullptr;

    fin_forceinline
    constexpr Node (Value_Type &&_value):
      value { move(_value) }
    {}
  };

  struct Iterator {
    Node *node;

    fin_forceinline
    constexpr Iterator (Node *_node): node { _node } {}

    fin_forceinline
    constexpr bool operator != (const Iterator &other) const {
      return this->node != other.node;
    }

    fin_forceinline
    constexpr Iterator& operator ++ () {
      node = node->next;
      return *this;
    }

    fin_forceinline
    constexpr Value_Type & operator * () { return node->value; }
  };

  Node  *first = nullptr;
  Node  *last  = nullptr;

  usize count = 0;
  
  fin_forceinline constexpr Iterator begin (this const auto &self) { return Iterator(self.first); } 
  fin_forceinline constexpr Iterator end   (this const auto &)     { return Iterator(nullptr); }

  fin_forceinline
  constexpr void for_each (const Invocable<void, T &> auto &func) const {
    for (auto node = first; node; node = node->next) func(node->value);
  }

  T * find (const Invocable<bool, const T &> auto &pred) const {
    for (auto node = first; node; node = node->next)
      if (pred(node->value)) return &node->value;
    return nullptr;
  }

  bool contains (const Invocable<bool, const T &> auto &pred) const {
    return !!find(pred);
  }
  
  bool contains (const T &value) const
    requires requires (const T &a, const T &b) { { a == b } -> Same_Types<bool>; }
  {
    return contains([&] (const T &elem) { return elem == value; });
  }

  bool remove (const Invocable<bool, const T &> auto &pred) {
    Node *previous = nullptr, *node = nullptr;
    for (auto cursor = first; cursor; cursor = cursor->next) {
      if (pred(cursor->value)) {
        node = cursor;
        break;
      }

      previous = cursor;
    }

    if (!node) return false;

    this->count -= 1;

    if (!previous) {
      fin_ensure(this->first == node);

      this->first = node->next;
      return true;
    }

    previous->next = node->next;
    return true;
  }

};

template <typename T>
using List_Value = typename List<T>::Value_Type;

template <typename T, typename Allocator>
static T & list_push (Allocator &allocator, List<T> &list, List_Value<T> &&value) {
  using Node_Type = typename List<T>::Node;

  auto node = new (allocator) Node_Type(move(value));

  if (list.first == nullptr) {
    fin_ensure(list.last == nullptr);
    list.first = node;
    list.last  = node;
  }
  else {
    list.last->next = node;
    list.last       = node;
  }

  list.count += 1;

  return node->value;
}

template <typename T, typename Allocator>
static T & list_push_copy (Allocator &allocator, List<T> &list, List_Value<T> value) {
  return list_push(allocator, list, move(value));
}

template <typename T, typename Allocator>
static T & list_push_front(Allocator &allocator, List<T> &list, List_Value<T> &&value) {
  using Node_Type = typename List<T>::Node;

  auto node = new (allocator) Node_Type(move(value));

  if (list.first == nullptr) {
    fin_ensure(list.last == nullptr);
    list.first = node;
    list.last  = node;
  }
  else {
    node->next = list.first;
    list.first = node;
  }

  list.count += 1;

  return node->value;
}

template <typename T, typename Allocator>
static T & list_push_front_copy (Allocator &allocator, List<T> &list, List_Value<T> value) {
  return list_push_front(allocator, list, move(value));
}

template <typename T>
constexpr bool is_empty (const List<T> &list) {
  return list.first == nullptr;
}

namespace iterator {

template <typename T>
constexpr usize count (const List<T> &list) {
  return list.count;
}

}

}
