//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <string>
#include "SioClientListener.hpp"
//=============================================================================
SioClientListener::SioClientListener() { connection_finish = false; }
//=============================================================================
SioClientListener::~SioClientListener() { connection_finish = false; }
//=============================================================================
void
SioClientListener::on_fail()
{
    std::cout << "SioClientListener.on_fail" << std::endl;
}
//=============================================================================
void
SioClientListener::on_connected()
{
    std::cout << "SioClientListener.on_connected" << std::endl;
    lock.lock();
    condition.notify_all();
    connection_finish = true;
    lock.unlock();
}
//=============================================================================
void
SioClientListener::on_close(sio::client::close_reason const& reason)
{
    std::cout << "SioClientListener.on_close" << std::endl;
}
//=============================================================================
