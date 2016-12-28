//=============================================================================
#include "VertCat.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf VertCat(Evaluator *eval, const ArrayOfVector &v)
    {
        ArrayOf res;
        switch (v.size())
        {
            case 0:
            {
                res = ArrayOf::emptyConstructor();
            }
            break;
            case 1:
            {
                res = v[0];
                res.ensureSingleOwner();
            }
            break;
            default:
            {
                res = v[0];
                res.ensureSingleOwner();
                for (size_t k = 1; k < v.size(); k++)
                {
                    ArrayOf arg2 = v[k];
                    res = OverloadBinaryOperator(eval, res, arg2, "vertcat");
                }
            }
            break;
        }
        return res;
    }
    //=============================================================================
}
//=============================================================================
