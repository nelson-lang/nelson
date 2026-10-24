//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowModelRegistry.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
NFlowModelRegistry&
NFlowModelRegistry::instance()
{
    static NFlowModelRegistry s_instance;
    return s_instance;
}
//=============================================================================
std::string
NFlowModelRegistry::autoName() const
{
    // Caller holds the lock.
    if (!m_byName.count("untitled")) {
        return "untitled";
    }
    for (int i = 1; i < 1000000; ++i) {
        std::string candidate = "untitled" + std::to_string(i);
        if (!m_byName.count(candidate)) {
            return candidate;
        }
    }
    return "untitled";
}
//=============================================================================
bool
NFlowModelRegistry::createEmpty(
    const std::string& requestedName, double& outHandle, std::string& outName, std::string& err)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    std::string name = requestedName.empty() ? autoName() : requestedName;
    if (m_byName.count(name)) {
        err = "A model named '" + name + "' is already loaded.";
        return false;
    }
    auto doc = std::make_unique<NFlowModelDocument>(name);
    double handle = m_nextHandle++;
    m_byName[name] = std::move(doc);
    m_handleToName[handle] = name;
    m_nameToHandle[name] = handle;
    outHandle = handle;
    outName = name;
    return true;
}
//=============================================================================
bool
NFlowModelRegistry::add(
    std::unique_ptr<NFlowModelDocument> doc, double& outHandle, std::string& err)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    const std::string name = doc->name();
    auto existing = m_nameToHandle.find(name);
    if (existing != m_nameToHandle.end()) {
        // A model with this name is already loaded: refresh its content in place so
        // re-loading an edited file takes effect and the existing handle stays valid.
        // Discarding the new document would leave callers simulating the stale model.
        m_byName[name] = std::move(doc);
        outHandle = existing->second;
        (void)err;
        return true;
    }
    double handle = m_nextHandle++;
    m_byName[name] = std::move(doc);
    m_handleToName[handle] = name;
    m_nameToHandle[name] = handle;
    outHandle = handle;
    (void)err;
    return true;
}
//=============================================================================
NFlowModelDocument*
NFlowModelRegistry::resolveByName(const std::string& name)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_byName.find(name);
    return (it == m_byName.end()) ? nullptr : it->second.get();
}
//=============================================================================
NFlowModelDocument*
NFlowModelRegistry::resolveByHandle(double handle)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_handleToName.find(handle);
    if (it == m_handleToName.end()) {
        return nullptr;
    }
    auto nit = m_byName.find(it->second);
    return (nit == m_byName.end()) ? nullptr : nit->second.get();
}
//=============================================================================
bool
NFlowModelRegistry::handleOf(const std::string& name, double& outHandle) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_nameToHandle.find(name);
    if (it == m_nameToHandle.end()) {
        return false;
    }
    outHandle = it->second;
    return true;
}
//=============================================================================
bool
NFlowModelRegistry::nameOfHandle(double handle, std::string& outName) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_handleToName.find(handle);
    if (it == m_handleToName.end()) {
        return false;
    }
    outName = it->second;
    return true;
}
//=============================================================================
double
NFlowModelRegistry::handleForBlock(const std::string& blockPath)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_pathToBlockHandle.find(blockPath);
    if (it != m_pathToBlockHandle.end()) {
        return it->second;
    }
    double handle = m_nextHandle++;
    m_blockHandleToPath[handle] = blockPath;
    m_pathToBlockHandle[blockPath] = handle;
    return handle;
}
//=============================================================================
bool
NFlowModelRegistry::resolveBlockHandle(double handle, std::string& blockPath) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_blockHandleToPath.find(handle);
    if (it == m_blockHandleToPath.end()) {
        return false;
    }
    blockPath = it->second;
    return true;
}
//=============================================================================
void
NFlowModelRegistry::invalidateBlockHandle(const std::string& blockPath)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_pathToBlockHandle.find(blockPath);
    if (it == m_pathToBlockHandle.end()) {
        return;
    }
    m_blockHandleToPath.erase(it->second);
    m_pathToBlockHandle.erase(it);
}
//=============================================================================
bool
NFlowModelRegistry::isLoaded(const std::string& name) const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_byName.count(name) != 0;
}
//=============================================================================
bool
NFlowModelRegistry::close(const std::string& name)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = m_byName.find(name);
    if (it == m_byName.end()) {
        return false;
    }
    m_byName.erase(it);
    auto hit = m_nameToHandle.find(name);
    if (hit != m_nameToHandle.end()) {
        m_handleToName.erase(hit->second);
        m_nameToHandle.erase(hit);
    }
    auto rootOf = [](const std::string& path) {
        auto pos = path.find('/');
        return pos == std::string::npos ? path : path.substr(0, pos);
    };
    for (auto bit = m_pathToBlockHandle.begin(); bit != m_pathToBlockHandle.end();) {
        if (rootOf(bit->first) == name) {
            m_blockHandleToPath.erase(bit->second);
            bit = m_pathToBlockHandle.erase(bit);
        } else {
            ++bit;
        }
    }
    if (rootOf(m_currentSystem) == name) {
        m_currentSystem.clear();
        m_currentBlock.clear();
    }
    return true;
}
//=============================================================================
std::vector<std::string>
NFlowModelRegistry::loadedNames() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    std::vector<std::string> names;
    names.reserve(m_byName.size());
    for (const auto& kv : m_byName) {
        names.push_back(kv.first);
    }
    return names;
}
//=============================================================================
void
NFlowModelRegistry::setCurrentSystem(const std::string& systemPath)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_currentSystem = systemPath;
}
//=============================================================================
std::string
NFlowModelRegistry::currentSystem() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_currentSystem;
}
//=============================================================================
void
NFlowModelRegistry::setCurrentBlock(const std::string& blockPath)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_currentBlock = blockPath;
}
//=============================================================================
std::string
NFlowModelRegistry::currentBlock() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_currentBlock;
}
//=============================================================================
void
NFlowModelRegistry::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_byName.clear();
    m_handleToName.clear();
    m_nameToHandle.clear();
    m_blockHandleToPath.clear();
    m_pathToBlockHandle.clear();
    m_nextHandle = 1.0;
    m_currentSystem.clear();
    m_currentBlock.clear();
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
