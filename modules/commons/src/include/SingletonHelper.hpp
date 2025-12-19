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
namespace Nelson {
//=============================================================================
template <typename T> class SingletonHelper
{
protected:
    SingletonHelper() = default;
    SingletonHelper(const SingletonHelper&) = delete;
    SingletonHelper&
    operator=(const SingletonHelper&)
        = delete;

public:
    static T*&
    getInstancePtr()
    {
        static T* m_pInstance = nullptr;
        return m_pInstance;
    }

public:
    static T*
    getInstance()
    {
        T*& instance = getInstancePtr();
        if (instance == nullptr) {
            instance = new T();
        }
        return instance;
    }

    static void
    destroy()
    {
        T*& instance = getInstancePtr();
        if (instance != nullptr) {
            delete instance;
            instance = nullptr;
        }
    }

    virtual ~SingletonHelper() = default;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
