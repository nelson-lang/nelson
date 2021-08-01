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
#include <efsw/efsw.hpp>
#include <boost/filesystem.hpp>
#include "MxGetExtension.hpp"
#include "FileWatcherManager.hpp"
//=============================================================================
namespace Nelson {
class UpdatePathListener : public efsw::FileWatchListener
{
private:
    //=============================================================================
    void
    appendIfNelsonFile(const std::string& dir, const std::string& filename)
    {
        boost::filesystem::path pf = boost::filesystem::path(filename);
        std::wstring file_extension = pf.extension().generic_wstring();
        if (file_extension == L".m" || file_extension == L"." + getMexExtension()) {
            boost::filesystem::path parent_dir = boost::filesystem::path(dir);
            FileWatcherManager::getInstance()->addPathToRefresh(
                parent_dir.generic_wstring());
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
}
//=============================================================================
