
#pragma once

#include "anyfin/result.hpp"
#include "anyfin/arena.hpp"

#include "eko.hpp"

struct Codegen_Error {
  
};

Fin::Result<Codegen_Error, void> codegen (const Source_File &file);
