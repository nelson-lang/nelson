//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "ClassdefWeakReference.hpp"
#include "HandleGenericObject.hpp"
#include "HandleState.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
#define CLASSDEF_WEAK_REFERENCE_STORAGE_CLASS "__classdef_weak_reference_storage__"
#define CLASSDEF_HANDLE_PLACEHOLDER_CLASS "nelson.lang.HandlePlaceholder"
#define CLASSDEF_ERROR_INVALID_WEAK_REFERENCE "Invalid weak reference."
//=============================================================================
namespace Nelson {
//=============================================================================
class WeakReferenceStorageObject : public HandleGenericObject
{
private:
    std::weak_ptr<HandleState> target;
    std::string fallbackCategory = CLASSDEF_HANDLE_PLACEHOLDER_CLASS;

public:
    WeakReferenceStorageObject()
        : HandleGenericObject(CLASSDEF_WEAK_REFERENCE_STORAGE_CLASS, this, false)
    {
    }

    void
    setTarget(const ArrayOf& value)
    {
        if (!value.isHandle() || !value.isScalar()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((2)),
                L"handle");
        }
        const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
        if (refs == nullptr || !refs[0]) {
            fallbackCategory = value.handleClassName();
            target.reset();
            return;
        }
        fallbackCategory = refs[0]->category();
        target = refs[0];
    }

    ArrayOf
    getTarget(bool requireValid)
    {
        HandleRef ref = target.lock();
        if (ref && ref->isValid()) {
            return ArrayOf::handle(ref->id());
        }
        if (requireValid) {
            Error(L"Nelson:interpreter:invalidWeakReference",
                L"Weak reference target is an invalid handle.");
        }
        Dimensions dims(1, 1);
        return ArrayOf::invalidHandle(fallbackCategory, dims);
    }
};
//=============================================================================
static WeakReferenceStorageObject*
weakReferenceStorageArg(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    auto* object = dynamic_cast<WeakReferenceStorageObject*>(value.asHandleScalar());
    if (object == nullptr) {
        RaiseError(
            _W(CLASSDEF_ERROR_INVALID_WEAK_REFERENCE), L"Nelson:interpreter:invalidWeakReference");
    }
    return object;
}
//=============================================================================
ArrayOfVector
evaluateClassdefWeakReference(const std::string& opName, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    // opName is one of __classdef_weak_reference_{new,get,valid_get,set}__.
    // Distinguish by the character at index 26 (after "__classdef_weak_reference_").
    const char selector = opName.size() > 26 ? opName[26] : '\0';
    if (selector == 'n') { // new
        retval << ArrayOf::handle(new WeakReferenceStorageObject());
        return retval;
    }
    if (argIn.empty()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    WeakReferenceStorageObject* storage = weakReferenceStorageArg(argIn[0]);
    if (selector == 's') { // set
        if (argIn.size() < 2) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((2)),
                L"handle");
        }
        storage->setTarget(argIn[1]);
        return retval;
    }
    if (selector == 'v') { // valid_get
        retval << storage->getTarget(true);
        return retval;
    }
    retval << storage->getTarget(false); // get
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
