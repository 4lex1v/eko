
#pragma once

#include "anyfin/arena.hpp"

#include "ast.hpp"

using Fin::Memory_Arena;

List<Node *> build_tree (Memory_Arena &arena, const char *buffer);
