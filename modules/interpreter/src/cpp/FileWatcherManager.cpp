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
#include "FileWatcherManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include <FileWatcher.h>
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
class UpdatePathListener : public FW::FileWatchListener
{
public:
    UpdatePathListener() = default;
    void
    handleFileAction(WatchID watchid, const FW::String& dir, const FW::String& filename,
        FW::Action action) override
    {
        switch (action) {
        case FW::Action::Add: {
            boost::filesystem::path pf = boost::filesystem::path(filename);
            std::string file_extension = boost::filesystem::extension(pf);
            if (file_extension == ".nlf") {
                boost::filesystem::path parent_dir = boost::filesystem::path(dir);
                PathFuncManager::getInstance()->rehash(parent_dir.generic_wstring());
                /*
                #ifdef _MSC_VER
                printf("Added: %ls\n", filename.c_str());
                #else
                printf("Added: %s\n", filename.c_str());
                #endif
                */
            }
        } break;
        case FW::Action::Delete: {
            boost::filesystem::path pf = boost::filesystem::path(filename);
            std::string file_extension = boost::filesystem::extension(pf);
            if (file_extension == ".nlf") {
                boost::filesystem::path parent_dir = boost::filesystem::path(dir);
                PathFuncManager::getInstance()->rehash(parent_dir.generic_wstring());
                /*
                #ifdef _MSC_VER
                printf("Delete: %ls\n", filename.c_str());
                #else
                printf("Delete: %s\n", filename.c_str());
                #endif
                */
            }
        } break;
        case FW::Action::Modified:
            boost::filesystem::path pf = boost::filesystem::path(filename);
            std::string file_extension = boost::filesystem::extension(pf);
            if (file_extension == ".nlf") {
                /*
                #ifdef _MSC_VER
                printf("Modified: %ls\n", filename.c_str());
                #else
                printf("Modified: %s\n", filename.c_str());
                #endif
                */
            }
            break;
        }
    }
};
//=============================================================================
FileWatcherManager* FileWatcherManager::m_pInstance = nullptr;
//=============================================================================
FileWatcherManager::FileWatcherManager()
{
    auto* tmp = new FW::FileWatcher();
    fileWatcher = (void*)tmp;
}
//=============================================================================
void
FileWatcherManager::release()
{
    auto* ptr = static_cast<FW::FileWatcher*>(fileWatcher);
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
void
FileWatcherManager::addWacth(const std::wstring& directory)
{
    auto* watcher = new UpdatePathListener();
    WatchID id = -1;
    try {
#ifdef _MSC_VER
        id = (static_cast<FW::FileWatcher*>(fileWatcher))->addWatch(directory, watcher);
#else
        id = ((FW::FileWatcher*)fileWatcher)->addWatch(wstring_to_utf8(directory), watcher);
#endif
    } catch (const FW::FWException&) {
    }
}
//=============================================================================
void
FileWatcherManager::removeWatch(const std::wstring& directory)
{
    try {
#ifdef _MSC_VER
        (static_cast<FW::FileWatcher*>(fileWatcher))->removeWatch(directory);
#else
        ((FW::FileWatcher*)fileWatcher)->removeWatch(wstring_to_utf8(directory));
#endif
    } catch (const FW::FWException&) {
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
} // namespace Nelson
//=============================================================================
