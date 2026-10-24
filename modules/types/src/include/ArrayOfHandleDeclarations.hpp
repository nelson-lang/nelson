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
// This declaration fragment is included inside class ArrayOf.
// Do not include it directly from other headers.
//=============================================================================
// function_handle array
//=============================================================================
/**
 * Construct function handle values.
 */
static ArrayOf
functionHandle(function_handle fptr);
static ArrayOf
functionHandle(const std::wstring& functionName, const std::wstring& anonymous);
/**
 * Return the function handle stored by a scalar value.
 */
[[nodiscard]] function_handle
asFunctionHandle() const;
/**
 * Return whether the class tag is function_handle.
 */
[[nodiscard]] bool
isFunctionHandle() const;
//=============================================================================
// handle array
//=============================================================================
/**
 * Return whether the value stores Nelson handle states.
 */
[[nodiscard]] bool
isHandle() const;
/**
 * Construct handle arrays from native wrappers or numeric handle ids.
 */
static ArrayOf
handle(HandleGenericObject* ptr);
static ArrayOf
handle(nelson_handle hl);
static ArrayOf
emptyHandle(const std::string& className, Dimensions& dim);
/**
 * Construct an invalid handle array that still carries class information.
 */
static ArrayOf
invalidHandle(const std::string& className, Dimensions& dim);
/**
 * Return whether a scalar handle exposes the named property.
 */
[[nodiscard]] bool
isHandleProperty(const std::wstring& propertyName) const;
[[nodiscard]] bool
isHandleProperty(const std::string& propertyName) const;
/**
 * Return whether a scalar handle exposes the named method.
 */
[[nodiscard]] bool
isHandleMethod(const std::wstring& methodName) const;
[[nodiscard]] bool
isHandleMethod(const std::string& methodName) const;
/**
 * Return the category of a scalar handle.
 */
[[nodiscard]] std::string
handleCategory() const;
/**
 * Return the class name exposed by a scalar handle.
 */
[[nodiscard]] std::string
handleClassName() const;
/**
 * Return the native handle wrapper stored by a scalar handle.
 */
[[nodiscard]] HandleGenericObject*
asHandleScalar() const;
//=============================================================================
// graphics object handle
//=============================================================================
/**
 * Return whether the value stores graphics object handles.
 */
[[nodiscard]] bool
isGraphicsObject() const;
[[nodiscard]] std::string
graphicsObjectClassName() const;
void
setGraphicsObjectClassName(const std::string& className);
void
refreshGraphicsObjectClassName();
/**
 * Construct a scalar graphics object handle value.
 */
static ArrayOf
graphicsObject(go_handle graphicsObjectHandle);
static ArrayOf
graphicsObject(go_handle graphicsObjectHandle, const std::string& className);
static ArrayOf
graphicsObject(const std::vector<go_handle>& graphicsObjectHandles, const Dimensions& dimensions,
    const std::string& className);
static ArrayOf
emptyGraphicsObject(const std::string& className, Dimensions& dim);
/**
 * Return the graphics object handle stored by a scalar value.
 */
[[nodiscard]] go_handle
asGraphicsObjectScalar() const;
//=============================================================================
