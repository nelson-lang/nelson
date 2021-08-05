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
#include <mutex>
#include <string>
#include <efsw/efsw.hpp>
#include <boost/filesystem.hpp>
#include "FileWatcherManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
#include "UpdatePathListener.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::mutex m_mutex;
//=============================================================================
FileWatcherManager* FileWatcherManager::m_pInstance = nullptr;
//=============================================================================
FileWatcherManager::FileWatcherManager()
{
    UpdatePathListener *_watcher = new UpdatePathListener();
    watcher = (void*)_watcher; 
    auto* tmp = new efsw::FileWatcher();
    tmp->watch();
    fileWatcher = (void*)tmp;
    watchIDsMap.clear();
    pathsToRefresh.clear();
}
//=============================================================================
void
FileWatcherManager::release()
{
    auto* ptr = static_cast<efsw::FileWatcher*>(fileWatcher);
    watchIDsMap.clear();
    pathsToRefresh.clear();
    if (ptr != nullptr) {
        delete ptr;
        fileWatcher = nullptr;
    }
    if (watcher) {
        UpdatePathListener* _watcher = (UpdatePathListener*)watcher; 
        delete _watcher;
        _watcher = nullptr;
        watcher = nullptr;
    }
}
//=============================================================================
FileWatcherManager*
FileWatcherManager::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new FileWatcherManager();
    }
    return m_pInstance;
}
//=============================================================================
static std::string
utf8UniformizePath(const std::wstring& directory)
{
    std::wstring uniformPath = directory;
    if ((directory.back() == L'/') || (directory.back() == L'\\')) {
        uniformPath.pop_back();
    }
    boost::filesystem::path path(uniformPath);
    path = boost::filesystem::absolute(path);
    std::string utf8UniformPath = wstring_to_utf8(path.generic_wstring());
#ifdef _MSC_VER
    utf8UniformPath = utf8UniformPath + "\\";
#else
    utf8UniformPath = utf8UniformPath + "/";
#endif
    return utf8UniformPath;
}
//=============================================================================
void
FileWatcherManager::addWatch(const std::wstring& directory)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    std::string uniformizedPath = utf8UniformizePath(directory);
    std::map<std::string, std::pair<long, int>>::iterator it = watchIDsMap.find(uniformizedPath);
    if (it == watchIDsMap.end()) {
        UpdatePathListener* _watcher = (UpdatePathListener*)watcher; 
        if (_watcher == nullptr) {
            _watcher = new UpdatePathListener();
            watcher = (void*)_watcher;
        }
        std::string uniformizedPath = utf8UniformizePath(directory);
        efsw::WatchID id = ((efsw::FileWatcher*)fileWatcher)->addWatch(uniformizedPath, _watcher, false);
        watchIDsMap.emplace(uniformizedPath, std::make_pair(id, 1));
    } else {
        it->second.second++;
    }
}
//=============================================================================
void
FileWatcherManager::removeWatch(const std::wstring& directory)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    if (directory.empty())
        return;
    std::string uniformizedPath = utf8UniformizePath(directory);
    std::map<std::string, std::pair<long, int>>::iterator it = watchIDsMap.find(uniformizedPath);
    if (it != watchIDsMap.end()) {
        if (it->second.second == 1) {
            ((efsw::FileWatcher*)fileWatcher)->removeWatch(it->second.first);
            watchIDsMap.erase(it);
        } else {
            it->second.second--;
        }
    }
}
//=============================================================================
void
FileWatcherManager::rehashDirectories()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    for (auto p : pathsToRefresh) {
        PathFuncManager::getInstance()->rehash(p);
    }
    pathsToRefresh.clear();
}
//=============================================================================
void
FileWatcherManager::addPathToRefresh(const std::wstring& directory)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    pathsToRefresh.push_back(directory);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
