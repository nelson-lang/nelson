//=============================================================================
#include "OverloadDisplay.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    void OverloadDisplay(Evaluator *eval, ArrayOf a)
    {
        Context *context = eval->getContext();
        FunctionDef *funcDef = nullptr;
        std::string OverloadName = ClassName(a) + "_disp";
        if (!context->lookupFunction(OverloadName, funcDef))
        {
            Error(eval, utf8_to_wstring(_("function") + " "+ OverloadName + " " + _("undefined.")));
        }
        ArrayOfVector argsIn;
        argsIn.push_back(a);
        int nargout = 1;
        ArrayOfVector res = funcDef->evaluateFunction(eval, argsIn, nargout);
    }
    //=============================================================================
}
//=============================================================================
