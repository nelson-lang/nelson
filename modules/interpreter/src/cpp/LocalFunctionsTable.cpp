//=============================================================================
#include "LocalFunctionsTable.hpp"
#include "BuiltInFunctionDef.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    LocalFunctionsTable::LocalFunctionsTable()
    {
        cachedLocalMacro.clear();
    }
    //=============================================================================
    LocalFunctionsTable::~LocalFunctionsTable()
    {
        cachedLocalMacro.clear();
    }
    //=============================================================================
    bool LocalFunctionsTable::find(const std::string key, FuncPtr& dest)
    {
        boost::unordered_map<std::string, FuncPtr>::const_iterator found = cachedLocalMacro.find(key);
        if (found != cachedLocalMacro.end())
        {
            dest = found->second;
            return true;
        }
        return false;
    }
    //=============================================================================
    bool LocalFunctionsTable::add(const std::string key, const FuncPtr val)
    {
        FuncPtr v = nullptr;
        if (find(key, v))
        {
            return false;
        }
        cachedLocalMacro.emplace(key, val);
        return true;
    }
    //=============================================================================
};
//=============================================================================
