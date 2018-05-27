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
#include "FilesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FilesManager::FilesManager(Interface* io)
{
    File* STD_IN = new File(false);
    STD_IN->setFilePointer((void*)io);
    STD_IN->setFileName(L"stdin");
    STD_IN->setFileMode(L"rb");
    addFile(STD_IN);
    File* STD_OUT = new File(false);
    STD_OUT->setFilePointer((void*)io);
    STD_OUT->setFileName(L"stdout");
    STD_OUT->setFileMode(L"wb");
    addFile(STD_OUT);
    File* STD_ERR = new File(false);
    STD_ERR->setFilePointer((void*)io);
    STD_ERR->setFileName(L"stderr");
    STD_ERR->setFileMode(L"wb");
    addFile(STD_ERR);
}
//=============================================================================
FilesManager::~FilesManager()
{
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i] != nullptr) {
            delete userFiles[i];
            userFiles[i] = nullptr;
        }
    }
    userFiles.clear();
}
//=============================================================================
int
FilesManager::getAvailableFileID()
{
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i] == nullptr) {
            return (int)i;
        }
    }
    userFiles.push_back(NULL);
    return (int)getNumberOfFiles() - 1;
}
//=============================================================================
bool
FilesManager::isOpened(int fileID)
{
    if (fileID < 0) {
        return false;
    }
    if ((size_t)fileID < getNumberOfFiles()) {
        if (userFiles[fileID]) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
FilesManager::isStdIn(int fileID)
{
    return (fileID == 0);
}
//=============================================================================
bool
FilesManager::isStdOut(int fileID)
{
    return (fileID == 1);
}
//=============================================================================
bool
FilesManager::isStdErr(int fileID)
{
    return (fileID == 2);
}
//=============================================================================
bool
FilesManager::isStdStream(int fileID)
{
    return (isStdErr(fileID) || isStdOut(fileID) || isStdIn(fileID));
}
//=============================================================================
bool
FilesManager::isOpened(std::wstring filenameToSearch)
{
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i] != nullptr) {
            if (userFiles[i]->getFileName() == filenameToSearch) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
int
FilesManager::addFile(File* userfile)
{
    if (userfile) {
        int pos = getAvailableFileID();
        userFiles[pos] = userfile;
        return pos;
    }
    return -1;
}
//=============================================================================
// file also closed when you removeFile
bool
FilesManager::removeFile(int no)
{
    if (no > 2) // stdout, stdin, sterr cannot be removed
    {
        if ((size_t)no < getNumberOfFiles()) {
            if (userFiles[no]) {
                delete userFiles[no];
                userFiles[no] = nullptr;
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
wstringVector
FilesManager::getFilenames()
{
    wstringVector listnames;
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i]) {
            listnames.push_back(userFiles[i]->getFileName());
        }
    }
    return listnames;
}
//=============================================================================
wstringVector
FilesManager::getMode()
{
    wstringVector listmodes;
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i]) {
            listmodes.push_back(userFiles[i]->getFileMode());
        }
    }
    return listmodes;
}
//=============================================================================
void*
FilesManager::getFilePointer(int no)
{
    if (no < 0) {
        return nullptr;
    }
    if ((size_t)no < getNumberOfFiles()) {
        if (userFiles[no]) {
            return userFiles[no]->getFilePointer();
        }
    }
    return nullptr;
}
//=============================================================================
File*
FilesManager::getFile(int no)
{
    if (no < 0) {
        return nullptr;
    }
    if ((size_t)no < getNumberOfFiles()) {
        return userFiles[no];
    }
    return nullptr;
}
//=============================================================================
File*
FilesManager::getFile(std::wstring filename)
{
    return nullptr;
}
//=============================================================================
size_t
FilesManager::getNumberOfFiles()
{
    return userFiles.size();
}
//=============================================================================
boost::container::vector<uint64>
FilesManager::getIDs()
{
    boost::container::vector<uint64> IDs;
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i]) {
            IDs.push_back((uint64)i);
        }
    }
    return IDs;
}
}
//=============================================================================