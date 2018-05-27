//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
namespace ElementaryFunctionsGateway {
    ArrayOfVector
    isequalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
}
} // namespace Nelson
