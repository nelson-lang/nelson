#include <algorithm>
#include "VariablesTable.hpp"
//=============================================================================
#define SYMTAB 4096*2
//=============================================================================
namespace Nelson {
    //=============================================================================
    VariablesTable::VariablesTable()
    {
        //variablesMap.reserve(SYMTAB);
        //lockedVariables.reserve(SYMTAB);
    }
    //=============================================================================
    VariablesTable::~VariablesTable()
    {
        variablesMap.clear();
        lockedVariables.clear();
    }
    //=============================================================================
    bool VariablesTable::findVariable(const key_type& key, value_type& dest)
    {
        boost::unordered_map<key_type, value_type>::iterator it = variablesMap.find(key);
        if (it != variablesMap.end())
        {
            dest = it->second;
            return true;
        }
        return false;
    }
    //=============================================================================
    bool VariablesTable::isVariable(const key_type& key)
    {
        if (variablesMap.count(key) > 0)
        {
            return true;
        }
        return false;
    }
    //=============================================================================
    bool VariablesTable::deleteVariable(const key_type& key)
    {
        if (!isLockedVariable(key))
        {
            if (isVariable(key))
            {
                variablesMap.erase(key);
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool VariablesTable::insertVariable(const key_type& key, const value_type& val)
    {
        if (lockedVariables.empty())
        {
            variablesMap[key] = val;
            return true;
        }
        else
        {
            // insert only in a not locked variable
            if (!isLockedVariable(key))
            {
                variablesMap[key] = val;
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    stringVector VariablesTable::getVariablesList()
    {
        stringVector retlist;
        for (auto it = variablesMap.begin(); it != variablesMap.end(); ++it)
        {
            retlist.push_back(it->first);
        }
        return retlist;
    }
    //=============================================================================
    bool VariablesTable::isLockedVariable(std::string key)
    {
        if (!lockedVariables.empty())
        {
            return (std::find(lockedVariables.begin(), lockedVariables.end(), key) != lockedVariables.end());
        }
        return false;
    }
    //=============================================================================
    bool VariablesTable::lockVariable(std::string key)
    {
        if (!isLockedVariable(key))
        {
            // ans cannot be locked
            if (key != "ans")
            {
                lockedVariables.push_back(key);
            }
            return true;
        }
        else
        {
            return true;
        }
        return false;
    }
    //=============================================================================
    bool VariablesTable::unlockVariable(std::string key)
    {
        if (isLockedVariable(key))
        {
            lockedVariables.erase(std::find(lockedVariables.begin(), lockedVariables.end(), key));
            return true;
        }
        return false;
    }
    //=============================================================================
    stringVector VariablesTable::getLockedVariables()
    {
        return lockedVariables;
    }
    //=============================================================================
}
//=============================================================================
