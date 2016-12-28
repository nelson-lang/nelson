//=============================================================================
#include "StackError.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    boost::container::vector<ErrorInfo> StackError(Evaluator *eval)
    {
        boost::container::vector<ErrorInfo> vectErrorInfo;
        stringVector outstack;
        size_t i = 0;
        while (i < eval->cstack.size())
        {
            if (eval->cstack[i].tokid == 0)
            {
                // This is a new line in the stack trace - we search forward
                // until we get the last line in the current function.  This
                // is the "branch point" for that function.
                size_t j = i + 1;
                while ((j < eval->cstack.size()) && (eval->cstack[j].cname == eval->cstack[i].cname)
                        && (eval->cstack[j].detail == eval->cstack[i].detail)
                        && (eval->cstack[j].tokid != 0))
                {
                    j++;
                }
                ErrorInfo ei;
                std::wstring filename = utf8_to_wstring(eval->cstack[j - 1].cname.c_str());
                std::wstring functionname = utf8_to_wstring(eval->cstack[j - 1].detail.c_str());
                int lineposition = eval->cstack[j - 1].tokid & 0x0000FFFF;
                int columposition = eval->cstack[j - 1].tokid >> 16;
                ei.set(filename, functionname, lineposition, columposition);
                vectErrorInfo.push_back(ei);
                i = j;
            }
            else
            {
                i++;
            }
        }
        return vectErrorInfo;
    }
    //=============================================================================
};
//=============================================================================
