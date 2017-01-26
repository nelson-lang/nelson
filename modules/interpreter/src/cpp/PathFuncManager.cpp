//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "AstManager.hpp"
#include "GetVariableEnvironment.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    PathFuncManager* PathFuncManager::m_pInstance = nullptr;
    //=============================================================================
	static std::ifstream & safegetline(std::ifstream &os, std::string &line)
	{
		std::string myline;
		if (getline(os, myline)) {
			if (myline.size() && myline[myline.size() - 1] == '\r')
			{
				line = myline.substr(0, myline.size() - 1);
			}
			else
			{
				line = myline;
			}
		}
		return os;
	}
	//=============================================================================
    PathFuncManager::PathFuncManager()
    {
        _userPath = nullptr;
        userpathCompute();
    }
    //=============================================================================
    PathFuncManager *PathFuncManager::getInstance()
    {
        if (m_pInstance == nullptr)
        {
            m_pInstance = new PathFuncManager();
        }
        return m_pInstance;
    }
    //=============================================================================
    void PathFuncManager::destroy()
    {
        clearUserPath();
        clear();
        cachedPathFunc.clear();
        if (m_pInstance)
        {
            delete m_pInstance;
            m_pInstance = nullptr;
        }
    }
    //=============================================================================
    void PathFuncManager::clear()
    {
        for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                delete pf;
                pf = nullptr;
            }
        }
        _pathFuncVector.clear();
    }
    //=============================================================================
    bool PathFuncManager::isPointerOnPathFunctionDef(FuncPtr ptr)
    {
        std::wstring functionName;
        return find(ptr->hashid, functionName);
    }
    //=============================================================================
    bool PathFuncManager::find(const std::string name, FuncPtr &ptr)
    {
        bool res = false;
        boost::unordered_map<std::string, FuncPtr>::const_iterator found = cachedPathFunc.find(name);
        if (found != cachedPathFunc.end())
        {
            ptr = found->second;
            res = true;
        }
        else
        {
            FileFunc *ff = nullptr;
            if (find(utf8_to_wstring(name), &ff))
            {
                if (ff)
                {
                    ptr = processFile(ff->getFilename());
                    if (ptr)
                    {
                        ptr->hashid = ff->getHashID();
                        cachedPathFunc.emplace(name, ptr);
                        res = true;
                    }
                }
            }
        }
        return res;
    }
    //=============================================================================
    bool PathFuncManager::find(const std::wstring functionName, FileFunc **ff)
    {
        bool res = false;
        if (_userPath)
        {
            res = _userPath->findFuncName(functionName, ff);
        }
        if (!res)
        {
            for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
            {
                PathFunc *pf = *it;
                if (pf)
                {
                    res = pf->findFuncName(functionName, ff);
                    if (res)
                    {
                        return res;
                    }
                }
            }
        }
        return res;
    }
    //=============================================================================
    bool PathFuncManager::find(const std::wstring functionName, std::wstring &filename)
    {
        bool res = false;
        if (_userPath)
        {
            res = _userPath->findFuncName(functionName, filename);
        }
        if (!res)
        {
            for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
            {
                PathFunc *pf = *it;
                if (pf)
                {
                    res = pf->findFuncName(functionName, filename);
                    if (res)
                    {
                        return res;
                    }
                }
            }
        }
        return res;
    }
    //=============================================================================
    bool PathFuncManager::find(const std::wstring functionName, wstringVector &filesname)
    {
        bool res = false;
        filesname.clear();
        std::wstring filename;
        if (_userPath)
        {
            res = _userPath->findFuncName(functionName, filename);
            if (res)
            {
                filesname.push_back(filename);
            }
        }
        for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                res = pf->findFuncName(functionName, filename);
                if (res)
                {
                    filesname.push_back(filename);
                }
            }
        }
        return (filesname.size() > 0);
    }
    //=============================================================================
    bool PathFuncManager::find(size_t hashid, std::wstring &functionname)
    {
        bool res = false;
        if (_userPath)
        {
            res = _userPath->findFuncByHash(hashid, functionname);
            if (res)
            {
                return res;
            }
        }
        for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                res = pf->findFuncByHash(hashid, functionname);
                if (res)
                {
                    return res;
                }
            }
        }
        return res;
    }
    //=============================================================================
    bool PathFuncManager::addPath(const std::wstring path, bool begin)
    {
        bool res = false;
        PathFunc *pf = new PathFunc(path);
        if (pf)
        {
            for (boost::container::vector<PathFunc *>::iterator it = _pathFuncVector.begin(); it != _pathFuncVector.end(); ++it)
            {
                PathFunc *pf = *it;
                if (pf)
                {
                    boost::filesystem::path p1 {pf->getPath()}, p2 {path};
                    if (boost::filesystem::equivalent(p1, p2))
                    {
                        return true;
                    }
                }
            }
            if (begin)
            {
                _pathFuncVector.insert(_pathFuncVector.begin(), pf);
            }
            else
            {
                _pathFuncVector.push_back(pf);
            }
            res = true;
        }
        return res;
    }
    //=============================================================================
    bool PathFuncManager::removePath(const std::wstring path)
    {
        bool res = false;
        for (boost::container::vector<PathFunc *>::iterator it = _pathFuncVector.begin(); it != _pathFuncVector.end(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                boost::filesystem::path p1 { pf->getPath() }, p2 { path };
                if (boost::filesystem::equivalent(p1, p2))
                {
                    _pathFuncVector.erase(it);
                    return true;
                }
            }
        }
        return res;
    }
    //=============================================================================
    wstringVector PathFuncManager::getPathNameVector()
    {
        wstringVector list;
        if (_userPath != nullptr)
        {
            list.push_back(_userPath->getPath());
        }
        for (boost::container::vector<PathFunc *>::iterator it = _pathFuncVector.begin(); it != _pathFuncVector.end(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                list.push_back(pf->getPath());
            }
        }
        return list;
    }
    //=============================================================================
    std::wstring PathFuncManager::getUserPath()
    {
        std::wstring _path = L"";
        if (_userPath != nullptr)
        {
#ifdef _MSC_VER
            _path = _userPath->getPath() + L";";
#else
            _path = _userPath->getPath() + L":";
#endif
        }
        return _path;
    }
    //=============================================================================
    bool PathFuncManager::setUserPath(const std::wstring path, bool saveToFile)
    {
        bool res = false;
        clearUserPath();
        _userPath = new PathFunc(path);
        if (saveToFile)
        {
            saveUserPathToFile();
        }
        return true;
    }
    //=============================================================================
    void PathFuncManager::clearUserPath(bool saveToFile)
    {
        if (_userPath != nullptr)
        {
            delete _userPath;
            _userPath = nullptr;
        }
        if (saveToFile)
        {
            saveUserPathToFile();
        }
    }
    //=============================================================================
    void PathFuncManager::resetUserPath()
    {
        std::wstring preferedUserPath = L"";
        std::wstring prefDir = getPreferencesPath();
        std::wstring userPathFile = prefDir + L"/userpath.conf";
        try
        {
            boost::filesystem::path p = userPathFile;
            boost::filesystem::remove(p);
        }
        catch (boost::filesystem::filesystem_error const &)
        {
        }
        userpathCompute();
    }
    //=============================================================================
    void PathFuncManager::rehash()
    {
        if (_userPath != nullptr)
        {
            _userPath->rehash();
        }
        for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                pf->rehash();
            }
        }
    }
    //=============================================================================
    void PathFuncManager::rehash(const std::wstring path)
    {
        if (_userPath != nullptr)
        {
            try
            {
                boost::filesystem::path p1 { _userPath->getPath() }, p2 { path };
                if (boost::filesystem::equivalent(p1, p2))
                {
                    _userPath->rehash();
                    return;
                }
            }
            catch (const boost::filesystem::filesystem_error& )
            {
            }
        }
        for (boost::container::vector<PathFunc *>::reverse_iterator it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
                try
                {
                    boost::filesystem::path p1 { pf->getPath() }, p2 { path };
                    if (boost::filesystem::equivalent(p1, p2))
                    {
                        pf->rehash();
                        return;
                    }
                }
                catch (const boost::filesystem::filesystem_error& )
                {
                }
            }
        }
    }
    //=============================================================================
    std::wstring PathFuncManager::getPathNameAsString()
    {
        std::wstring p = L"";
        if (_userPath != nullptr)
        {
            if (_userPath->getPath() != L"")
            {
#ifdef _MSC_VER
                p = _userPath->getPath() + L";";
#else
                p = _userPath->getPath() + L":";
#endif
            }
        }
        for (boost::container::vector<PathFunc *>::iterator it = _pathFuncVector.begin(); it != _pathFuncVector.end(); ++it)
        {
            PathFunc *pf = *it;
            if (pf)
            {
#ifdef _MSC_VER
                p = p + pf->getPath() + L";";
#else
                p = p + pf->getPath() + L":";
#endif
            }
        }
#ifdef _MSC_VER
        if (boost::algorithm::ends_with(p, L";"))
#else
        if (boost::algorithm::ends_with(p, L":"))
#endif
        {
            p.pop_back();
        }
        return p;
    }
    //=============================================================================
    MacroFunctionDef *PathFuncManager::processFile(std::wstring nlf_filename)
    {
        MacroFunctionDef *fptr = nullptr;
        FILE *fr = nullptr;
#ifdef _MSC_BUILD
        fr = _wfopen(nlf_filename.c_str(), L"rt");
#else
        fr = fopen(wstring_to_utf8(nlf_filename).c_str(), "rt");
#endif
        if (!fr)
        {
			throw Exception(_W("Cannot open:") + L" " + nlf_filename);
		}
        ParserState pstate = ParseError;
        resetAstBackupPosition();
        boost::container::vector<ASTPtr> ptAst;
        try
        {
            pstate = parseFile(fr, wstring_to_utf8(nlf_filename).c_str());
            ptAst = getAstUsed();
        }
        catch (const Exception &)
        {
            deleteAstVector(ptAst);
            resetAstBackupPosition();
            fclose(fr);
            throw;
        }
        fclose(fr);
        if (pstate != FuncDef)
        {
            deleteAstVector(ptAst);
            resetAstBackupPosition();
            throw Exception(_W("a valid function definition expected.") + std::wstring(L"\n") + nlf_filename);
        }
        try
        {
            fptr = getParsedFunctionDef();
        }
        catch (const Exception &)
        {
            throw Exception(_W("a valid function definition expected.") + std::wstring(L"\n") + nlf_filename);
        }
        if (fptr == nullptr)
        {
            throw Exception(_W("a valid function definition expected.") + std::wstring(L"\n") + nlf_filename);
        }
        fptr->ptAst = ptAst;
        resetAstBackupPosition();
        boost::filesystem::path pathFunction(nlf_filename);
        const std::string functionNameFromFile = pathFunction.stem().generic_string();
        if (!boost::iequals(functionNameFromFile, fptr->name))
        {
            std::string name = fptr->name;
            delete fptr;
            fptr = nullptr;
            throw Exception(_("filename and function name are not same (") + name + _(" vs ") + functionNameFromFile + "). " + _("function not loaded."));
        }
        return fptr;
    }
    //=============================================================================
    void PathFuncManager::clearCache()
    {
        for (boost::unordered_map<std::string, FuncPtr>::iterator iter = cachedPathFunc.begin(); iter != cachedPathFunc.end(); ++iter)
        {
            MacroFunctionDef* f = (MacroFunctionDef*)iter->second;
            if (f != nullptr)
            {
                delete f;
                f = nullptr;
            }
        }
        cachedPathFunc.clear();
    }
    //=============================================================================
    void PathFuncManager::clearCache(stringVector exceptedFunctions)
    {
        boost::unordered_map<std::string, FuncPtr> backup;
        for (boost::unordered_map<std::string, FuncPtr>::iterator iter = cachedPathFunc.begin(); iter != cachedPathFunc.end(); ++iter)
        {
            MacroFunctionDef* f = (MacroFunctionDef*)iter->second;
            if (f != nullptr)
            {
                stringVector::iterator it = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), f->name);
                if (it == exceptedFunctions.end())
                {
                    delete f;
                    f = nullptr;
                }
                else
                {
                    backup.emplace(f->name, f);
                }
            }
        }
        cachedPathFunc.clear();
        cachedPathFunc = backup;
    }
    //=============================================================================
    bool PathFuncManager::isDir(std::wstring pathname)
    {
        boost::filesystem::path data_dir(pathname);
        bool bRes = false;
        try
        {
            bRes = boost::filesystem::exists(data_dir) && boost::filesystem::is_directory(data_dir);
        }
        catch (const boost::filesystem::filesystem_error& e)
        {
            if (e.code() == boost::system::errc::permission_denied)
            {
            }
            bRes = false;
        }
        return bRes;
    }
    //=============================================================================
    bool PathFuncManager::isFile(std::wstring filename)
    {
        boost::filesystem::path data_dir(filename);
        bool bRes = false;
        try
        {
            bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
        }
        catch (const boost::filesystem::filesystem_error& e)
        {
            if (e.code() == boost::system::errc::permission_denied)
            {
            }
            bRes = false;
        }
        return bRes;
    }
    //=============================================================================
    void PathFuncManager::userpathCompute()
    {
        clearUserPath();
        std::wstring userpathEnv = GetVariableEnvironment(L"NELSON_USERPATH", L"");
        bool bSet = false;
        if (userpathEnv != L"")
        {
            if (isDir(userpathEnv))
            {
                setUserPath(userpathEnv);
                bSet = true;
            }
        }
        if (!bSet)
        {
            std::wstring preferedUserPath = L"";
            std::wstring prefDir;
            std::wstring userPathFile;
            try
            {
                prefDir = getPreferencesPath();
            }
            catch (Exception &)
            {
                prefDir = L"";
            }
            try
            {
                userPathFile = prefDir + L"/userpath.conf";
                bool bIsFile = boost::filesystem::exists(userPathFile) && !boost::filesystem::is_directory(userPathFile);
                if (bIsFile)
                {
                    std::wstring preferedUserPath = loadUserPathFromFile();
                    if (preferedUserPath != L"")
                    {
                        if (isDir(preferedUserPath))
                        {
                            setUserPath(preferedUserPath);
                            bSet = true;
                        }
                    }
                    else
                    {
                        bSet = true;
                    }
                }
            }
            catch (const boost::filesystem::filesystem_error&)
            {
            }
        }
        if (!bSet)
        {
#ifdef _MSC_VER
            std::wstring userprofileEnv = GetVariableEnvironment(L"USERPROFILE", L"");
            if (userprofileEnv != L"")
            {
                std::wstring userpathDir = userprofileEnv + std::wstring(L"/Documents/Nelson");
                if (!isDir(userpathDir))
                {
                    try
                    {
                        boost::filesystem::create_directories(userpathDir);
                    }
                    catch (const boost::filesystem::filesystem_error &)
                    {
                    }
                }
                if (isDir(userpathDir))
                {
                    setUserPath(userpathDir);
                    bSet = true;
                }
            }
#else
            std::wstring homeEnv = GetVariableEnvironment(L"HOME", L"");
            if (homeEnv != L"")
            {
                std::wstring userpathDir = homeEnv + std::wstring(L"/Documents/Nelson");
                if (!isDir(userpathDir))
                {
                    try
                    {
                        boost::filesystem::create_directories(userpathDir);
                    }
                    catch (const boost::filesystem::filesystem_error &)
                    {
                    }
                }
                if (isDir(userpathDir))
                {
                    setUserPath(userpathDir);
                    bSet = true;
                }
            }
#endif
        }
    }
    //=============================================================================
    std::wstring PathFuncManager::getPreferencesPath()
    {
#define NELSON_PREFERENCES_PATH_ENV L"NELSON_PREFERENCES_PATH"
        std::wstring prefPath = GetVariableEnvironment(NELSON_PREFERENCES_PATH_ENV, L"");
        return prefPath;
    }
    //=============================================================================
    std::wstring PathFuncManager::loadUserPathFromFile()
    {
        std::wstring preferedUserPath = L"";
        std::wstring prefDir = getPreferencesPath();
        std::wstring userPathFile = prefDir + L"/userpath.conf";
        bool bIsFile = boost::filesystem::exists(userPathFile) && !boost::filesystem::is_directory(userPathFile);
        if (bIsFile)
        {
            std::string jsonString = "";
            std::string tmpline;
#ifdef _MSC_VER
            std::ifstream jsonFile(userPathFile);
#else
            std::ifstream jsonFile(wstring_to_utf8(userPathFile));
#endif
            if (jsonFile.is_open())
            {
                while (safegetline(jsonFile, tmpline))
                {
                    jsonString += tmpline + '\n';
                }
                jsonFile.close();
                boost::property_tree::ptree pt;
                std::istringstream is(jsonString);
                try
                {
                    boost::property_tree::read_json(is, pt);
                    preferedUserPath = utf8_to_wstring(pt.get<std::string>("userpath"));
                }
                catch (boost::property_tree::json_parser::json_parser_error &je)
                {
                    je.message();
                }
            }
        }
        return preferedUserPath;
    }
    //=============================================================================
    bool PathFuncManager::saveUserPathToFile()
    {
        std::wstring up = L"";
        if (_userPath != nullptr)
        {
            up = _userPath->getPath();
        }
        std::wstring preferedUserPath = L"";
        std::wstring prefDir = getPreferencesPath();
        std::wstring userPathFile = prefDir + L"/userpath.conf";
        boost::property_tree::ptree pt;
        pt.put("userpath", wstring_to_utf8(up));
        std::ostringstream buf;
        boost::property_tree::write_json(buf, pt, false);
        std::string json = buf.str();
#ifdef _MSC_VER
        std::ofstream out(userPathFile);
#else
        std::ofstream out(wstring_to_utf8(userPathFile));
#endif
        out << json;
        out.close();
        return true;
    }
    //=============================================================================
}
//=============================================================================
