//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <sio_socket.h>
#include <sio_client.h>
#include "nlsSio_client_exports.h"
#include "SioClientListener.hpp"
//=============================================================================
namespace Nelson {
class NLSSIO_CLIENT_IMPEXP SioClientCommand
{
public:
    static SioClientCommand*
    getInstance();
    void
    reply(const std::string& stringToReply);
    std::string
    getCommand();
    void
    updateCommand(const std::string& command);
    bool
    isInitialized();
    bool
    create(const std::string& ipAddress);
    void
    clc();
    void
    available();
    void
    unavailable();
    void
    sioemit(const std::string& name, const std::string& message);
    void
    sioregister(const std::string& name, const std::string& function_name);
    void
    siounregister(const std::string& name);
    void
    quit();
    void
    promptUpdated(const std::string& prompt);

private:
    std::string _command;
    std::string _ipAddress;
    sio::client _sioClient;
    SioClientListener _sioClientListener;
    bool _initialized;

    SioClientCommand();
    bool
    createConnection(const std::string& ipAddress);
    static SioClientCommand* m_pInstance;
    static sio::socket::ptr _socket;
};
} // namespace Nelson
//=============================================================================
