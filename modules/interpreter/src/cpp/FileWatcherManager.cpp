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
#include <vector>
#include "FileWatcherManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<std::wstring> pathsToHash;
static std::mutex m_mutex;
//=============================================================================
class UpdatePathListener : public efsw::FileWatchListener
{
private:
    //=============================================================================
    void
    appendIfNelsonFile(const std::string& dir, const std::string& filename)
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        boost::filesystem::path pf = boost::filesystem::path(filename);
        std::wstring file_extension = pf.extension().generic_wstring();
        if (file_extension == L".m" || file_extension == L"." + getMexExtension()) {
            boost::filesystem::path parent_dir = boost::filesystem::path(dir);
            pathsToHash.push_back(parent_dir.generic_wstring());
        }
    }
    //=============================================================================
public:
    //=============================================================================
    UpdatePathListener() = default;
    //=============================================================================
    void
    handleFileAction(efsw::WatchID watchid, const std::string& dir, const std::string& filename,
        efsw::Action action, std::string oldFilename = "") override
    {
        switch (action) {
        case efsw::Action::Add: {
            appendIfNelsonFile(dir, filename);
        } break;
        case efsw::Action::Delete: {
            appendIfNelsonFile(dir, filename);
        } break;
        case efsw::Action::Moved: {
            appendIfNelsonFile(dir, filename);
        } break;
        default:
        case efsw::Action::Modified: {
            // nothing to do
            // modified managed by timestamp
        } break;
        }
    }
    //=============================================================================
};
//=============================================================================
FileWatcherManager* FileWatcherManager::m_pInstance = nullptr;
//=============================================================================
FileWatcherManager::FileWatcherManager()
{
    auto* tmp = new efsw::FileWatcher();
    tmp->watch();
    fileWatcher = (void*)tmp;
}
//=============================================================================
void
FileWatcherManager::release()
{
    auto* ptr = static_cast<efsw::FileWatcher*>(fileWatcher);
    if (ptr != nullptr) {
        delete ptr;
        fileWatcher = nullptr;
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
    auto* watcher = new UpdatePathListener();
    efsw::WatchID id = -1;
    std::string uniformizedPath = utf8UniformizePath(directory);
    std::list<std::string> directories = ((efsw::FileWatcher*)fileWatcher)->directories();
    for (auto dir : directories) {
        if (uniformizedPath == dir) {
            return;
        }
    }
    id = ((efsw::FileWatcher*)fileWatcher)->addWatch(uniformizedPath, watcher);
}
//=============================================================================
void
FileWatcherManager::removeWatch(const std::wstring& directory)
{
    ((efsw::FileWatcher*)fileWatcher)->removeWatch(utf8UniformizePath(directory));
}
//=============================================================================
void
FileWatcherManager::rehashDirectories()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    for (auto p : pathsToHash) {
        PathFuncManager::getInstance()->rehash(p);
    }
    pathsToHash.clear();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
