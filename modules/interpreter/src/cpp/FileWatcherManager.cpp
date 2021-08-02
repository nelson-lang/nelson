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
#include <FileWatcher.h>
#include <boost/filesystem.hpp>
#include "FileWatcherManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "UpdatePathListener.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FileWatcherManager* FileWatcherManager::m_pInstance = nullptr;
//=============================================================================
static std::wstring
uniformizePath(const std::wstring& directory)
{
    std::wstring uniformPath = directory;
    if ((directory.back() == L'/') || (directory.back() == L'\\')) {
        uniformPath.pop_back();
    }
    boost::filesystem::path path(uniformPath);
    path = boost::filesystem::absolute(path);
    uniformPath = path.generic_wstring();
#ifdef _MSC_VER
    uniformPath = uniformPath + L"\\";
#else
    uniformPath = uniformPath + L"/";
#endif
    return uniformPath;
}
//=============================================================================
FileWatcherManager::FileWatcherManager()
{
    UpdatePathListener* _fileListener = new UpdatePathListener();
    fileListener = static_cast<void*>(_fileListener);
    FW::FileWatcher* _fileWatcher = new FW::FileWatcher();
    fileWatcher = static_cast<void*>(_fileWatcher);
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
void
FileWatcherManager::addWatch(const std::wstring& directory)
{
    std::wstring uniformizedPath = uniformizePath(directory);
    std::map<std::wstring, std::pair<long, int>>::iterator it = watchIDsMap.find(uniformizedPath);
    if (it == watchIDsMap.end()) {
        UpdatePathListener* _fileListener = static_cast<UpdatePathListener*>(fileListener);
        WatchID id;
        try {
#ifdef _MSC_VER
            id = (static_cast<FW::FileWatcher*>(fileWatcher))->addWatch(directory, _fileListener);
#else
            id = ((FW::FileWatcher*)fileWatcher)
                     ->addWatch(wstring_to_utf8(directory), _fileListener);
#endif
        } catch (const FW::FWException&) {
            id = -1;
        }
        if (id != -1) {
            watchIDsMap.emplace(uniformizedPath, std::make_pair(id, 1));
        }
    } else {
        it->second.second++;
    }
}
//=============================================================================
void
FileWatcherManager::removeWatch(const std::wstring& directory)
{
    std::wstring uniformizedPath = uniformizePath(directory);
    std::map<std::wstring, std::pair<long, int>>::iterator it = watchIDsMap.find(uniformizedPath);
    if (it != watchIDsMap.end()) {
        if (it->second.second == 1) {
            ((FW::FileWatcher*)fileWatcher)->removeWatch(it->second.first);
            watchIDsMap.erase(it);
        } else {
            it->second.second--;
        }
    }
}
//=============================================================================
void
FileWatcherManager::update()
{
    try {
        (static_cast<FW::FileWatcher*>(fileWatcher))->update();
    } catch (const FW::FWException&) {
    }
}
//=============================================================================
void
FileWatcherManager::release()
{
    UpdatePathListener* _fileListener = static_cast<UpdatePathListener*>(fileListener);
    if (_fileListener) {
        delete _fileListener;
        _fileListener = nullptr;
        fileListener = nullptr;
    }
    FW::FileWatcher* _fileWatcher = static_cast<FW::FileWatcher*>(fileWatcher);
    if (_fileWatcher != nullptr) {
        delete _fileWatcher;
        _fileWatcher = nullptr;
        fileWatcher = nullptr;
    }
    pathsToRefresh.clear();
    watchIDsMap.clear();
}
//=============================================================================
void
FileWatcherManager::addPathToRefreshList(const std::wstring& directory)
{
    pathsToRefresh.push_back(directory);
}
//=============================================================================
wstringVector
FileWatcherManager::getPathToRefresh(bool withClear)
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
