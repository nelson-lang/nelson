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
#pragma once
//=============================================================================
#include <string>
#include <boost/filesystem.hpp>
#include <FileWatcher.h>
#include "FileWatcherManager.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
class UpdatePathListener : public FW::FileWatchListener
{
private:
    //=============================================================================
    void
    appendIfNelsonFile(const FW::String& dir, const FW::String& filename)
    {
        boost::filesystem::path pf = boost::filesystem::path(filename);
        std::wstring file_extension = pf.extension().generic_wstring();
        if (file_extension == L".m" || file_extension == L"." + Nelson::getMexExtension()) {
            boost::filesystem::path parent_dir = boost::filesystem::path(dir);
            Nelson::FileWatcherManager::getInstance()->addPathToRefreshList(
                parent_dir.generic_wstring());
        }
    }
    //=============================================================================
public:
    UpdatePathListener() = default;
    void
    handleFileAction(FW::WatchID watchid, const FW::String& dir, const FW::String& filename,
        FW::Action action) override
    {
        switch (action) {
        case FW::Action::Add: {
            appendIfNelsonFile(dir, filename);
        } break;
        case FW::Action::Delete: {
            appendIfNelsonFile(dir, filename);
        } break;
        case FW::Action::Modified: {
        } break;
        }
    }
};
//=============================================================================
