//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    STD_IN->setEncoding(L"UTF-16");
    STD_IN->setMachineFormat(L"native");
    addFile(STD_IN);
    File* STD_OUT = new File(false);
    STD_OUT->setFilePointer((void*)io);
    STD_OUT->setFileName(L"stdout");
    STD_OUT->setFileMode(L"wb");
    STD_OUT->setEncoding(L"UTF-16");
    STD_OUT->setMachineFormat(L"native");
    addFile(STD_OUT);
    File* STD_ERR = new File(false);
    STD_ERR->setFilePointer((void*)io);
    STD_ERR->setFileName(L"stderr");
    STD_ERR->setFileMode(L"wb");
    STD_ERR->setEncoding(L"UTF-16");
    STD_ERR->setMachineFormat(L"native");
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
            return static_cast<int>(i);
        }
    }
    userFiles.push_back(NULL);
    return static_cast<int>(getNumberOfFiles()) - 1;
}
//=============================================================================
bool
FilesManager::isOpened(int fileID)
{
    if (fileID < 0) {
        return false;
    }
    if (static_cast<size_t>(fileID) < getNumberOfFiles()) {
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
FilesManager::isOpened(const std::wstring& filenameToSearch)
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
        if (static_cast<size_t>(no) < getNumberOfFiles()) {
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
    if (static_cast<size_t>(no) < getNumberOfFiles()) {
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
    if (static_cast<size_t>(no) < getNumberOfFiles()) {
        return userFiles[no];
    }
    return nullptr;
}
//=============================================================================
File*
FilesManager::getFile(const std::wstring& filename)
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
std::vector<uint64>
FilesManager::getIDs()
{
    std::vector<uint64> IDs;
    for (size_t i = 0; i < getNumberOfFiles(); i++) {
        if (userFiles[i]) {
            IDs.push_back((uint64)i);
        }
    }
    return IDs;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
