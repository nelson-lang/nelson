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
#include "Nelson_VERSION.h"
//=============================================================================
// Scope shared by every QSettings instance of the application.
// It must not depend on the version: a version-based scope creates a new store
// on each release and resets the window layout and the user preferences.
// NELSON_SETTINGS_VERSION is stored in the settings so that a future release
// can migrate or discard an incompatible store.
//=============================================================================
#define NELSON_SETTINGS_ORGANIZATION NELSON_PRODUCT_NAME
#define NELSON_SETTINGS_APPLICATION NELSON_PRODUCT_NAME
#define NELSON_SETTINGS_VERSION_KEY "settingsVersion"
#define NELSON_SETTINGS_VERSION 1
//=============================================================================
