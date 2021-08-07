//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include "DirectoryWatcherManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const std::wstring mexExtension = getMexExtension();
static const std::wstring macroExtension = L".m";
//=============================================================================
DirectoryWatcherManager* DirectoryWatcherManager::m_pInstance = nullptr;
//=============================================================================
static std::wstring
uniformizePath(const std::wstring& directory)
{
    std::wstring uniformPath = directory;
    if (directory.empty()) {
        return uniformPath;
    }
    if ((directory.back() == L'/') || (directory.back() == L'\\')) {
        uniformPath.pop_back();
    }
    boost::filesystem::path path(uniformPath);
    path = boost::filesystem::absolute(path);
    uniformPath = path.generic_wstring();
    uniformPath = uniformPath + L"/";
    return uniformPath;
}
//=============================================================================
DirectoryWatcherManager::DirectoryWatcherManager()
{
    directoriesWatched.clear();
    pathsToRefresh.clear();
}
//=============================================================================
DirectoryWatcherManager*
DirectoryWatcherManager::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new DirectoryWatcherManager();
    }
    return m_pInstance;
}
//=============================================================================
void
DirectoryWatcherManager::addWatch(const std::wstring& directory)
{
    std::wstring uniformizedPath = uniformizePath(directory);
    std::vector<std::pair<std::wstring, int>>::iterator it;
    for (it = directoriesWatched.begin(); it != directoriesWatched.end(); ++it) {
        if (it->first == uniformizedPath) {
            it->second++;
            return;
        }
    }
    if (it == directoriesWatched.end()) {
        directoriesWatched.push_back(std::make_pair(uniformizedPath, 1));
    }
}
//=============================================================================
void
DirectoryWatcherManager::removeWatch(const std::wstring& directory)
{
    std::wstring uniformizedPath = uniformizePath(directory);
    std::vector<std::pair<std::wstring, int>>::reverse_iterator it;
    for (it = directoriesWatched.rbegin(); it != directoriesWatched.rend(); ++it) {
        if (it->first == uniformizedPath) {
            if (it->second == 1) {
                directoriesWatched.erase(std::next(it).base());
            } else {
                it->second--;
            }
            return;
        }
    }
}
//=============================================================================
static bool
isfile(const std::wstring &filename)
{
    try {
        return boost::filesystem::exists(filename) && !boost::filesystem::is_directory(filename);
    } catch (const boost::filesystem::filesystem_error&) { }
    return false;
}
//=============================================================================
void
DirectoryWatcherManager::update(const std::wstring& functionName)
{
    const std::wstring macroFilename = functionName + macroExtension;
    const std::wstring mexFilename =  functionName + mexExtension;
 
    std::vector<std::pair<std::wstring, int>>::reverse_iterator it;
    for (it = directoriesWatched.rbegin(); it != directoriesWatched.rend(); ++it) {
        const std::wstring macroFullFilename = it->first + macroFilename;
        const std::wstring mexFullFilename = it->first + mexFilename;
        if (isfile(mexFullFilename) || isfile(macroFullFilename)) {
            pathsToRefresh.push_back(it->first);
            return;
        }
    }
}
//=============================================================================
void
DirectoryWatcherManager::release()
{
    directoriesWatched.clear();
    pathsToRefresh.clear();
}
//=============================================================================
void
DirectoryWatcherManager::addPathToRefreshList(const std::wstring& directory)
{
    pathsToRefresh.push_back(directory);
}
//=============================================================================
wstringVector
DirectoryWatcherManager::getPathToRefresh(bool withClear)
{
    wstringVector paths = pathsToRefresh;
    if (withClear) {
        pathsToRefresh.clear();
    }
    return paths;
}
//=============================================================================
}
//=============================================================================
