//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
    UpdatePathListener() {}
    void
    handleFileAction(
        WatchID watchid, const FW::String& dir, const FW::String& filename, FW::Action action)
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
FileWatcherManager::FileWatcherManager() { fileWatcher = (void*)new FW::FileWatcher(); }
//=============================================================================
void
FileWatcherManager::release()
{
    FW::FileWatcher* ptr = (FW::FileWatcher*)fileWatcher;
    if (ptr) {
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
FileWatcherManager::addWacth(std::wstring directory)
{
    UpdatePathListener* watcher = new UpdatePathListener();
    WatchID id = -1;
    try {
#ifdef _MSC_VER
        id = ((FW::FileWatcher*)fileWatcher)->addWatch(directory, watcher);
#else
        id = ((FW::FileWatcher*)fileWatcher)->addWatch(wstring_to_utf8(directory), watcher);
#endif
    } catch (const FW::FWException&) {
    }
}
//=============================================================================
void
FileWatcherManager::removeWatch(std::wstring directory)
{
    try {
#ifdef _MSC_VER
        ((FW::FileWatcher*)fileWatcher)->removeWatch(directory);
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
        ((FW::FileWatcher*)fileWatcher)->update();
    } catch (const FW::FWException&) {
    }
}
//=============================================================================
}
//=============================================================================
