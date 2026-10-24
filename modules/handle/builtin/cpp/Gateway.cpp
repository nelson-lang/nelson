//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefMixinNames.hpp"
#include "OverloadName.hpp"
//=============================================================================
#include "classdefHandleBuiltin.hpp"
#include "deleteBuiltin.hpp"
#include "enumerationBuiltin.hpp"
#include "eventsBuiltin.hpp"
#include "getBuiltin.hpp"
#include "invokeBuiltin.hpp"
#include "ismethodBuiltin.hpp"
#include "ispropBuiltin.hpp"
#include "isvalidBuiltin.hpp"
#include "metaclassBuiltin.hpp"
#include "metaPackageBuiltin.hpp"
#include "superclassesBuiltin.hpp"
#include "methodsBuiltin.hpp"
#include "cancelBuiltin.hpp"
#include "propertiesBuiltin.hpp"
#include "setBuiltin.hpp"
//=============================================================================
#include "handle_eqBuiltin.hpp"
#include "handle_deleteBuiltin.hpp"
#include "handle_displayBuiltin.hpp"
#include "handle_fieldnamesBuiltin.hpp"
#include "handle_getBuiltin.hpp"
#include "handle_invokeBuiltin.hpp"
#include "handle_isequalBuiltin.hpp"
#include "handle_ismethodBuiltin.hpp"
#include "handle_ispropBuiltin.hpp"
#include "handle_isvalidBuiltin.hpp"
#include "handle_methodsBuiltin.hpp"
#include "handle_propertiesBuiltin.hpp"
#include "handle_setBuiltin.hpp"
#include "handle_vertcatBuiltin.hpp"
#include "handle_horzcatBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { "addlistener", (ptrBuiltin)Nelson::HandleGateway::classdefAddListenerBuiltin, 1, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "listener", (ptrBuiltin)Nelson::HandleGateway::classdefListenerBuiltin, 1, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "notify", (ptrBuiltin)Nelson::HandleGateway::classdefNotifyBuiltin, 0, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "delete", (ptrBuiltin)Nelson::HandleGateway::deleteBuiltin, 0, 1 },
    { "get", (ptrBuiltin)Nelson::HandleGateway::getBuiltin, 1, 1 },
    { "set", (ptrBuiltin)Nelson::HandleGateway::setBuiltin, 1, 1 },
    { "invoke", (ptrBuiltin)Nelson::HandleGateway::invokeBuiltin, 1, 2 },
    { "isvalid", (ptrBuiltin)Nelson::HandleGateway::isvalidBuiltin, 1, 1 },
    { "methods", (ptrBuiltin)Nelson::HandleGateway::methodsBuiltin, 1, 1 },
    { "properties", (ptrBuiltin)Nelson::HandleGateway::propertiesBuiltin, 1, 1 },
    { "isprop", (ptrBuiltin)Nelson::HandleGateway::ispropBuiltin, 1, 2 },
    { "ismethod", (ptrBuiltin)Nelson::HandleGateway::ismethodBuiltin, 1, 2 },
    { "cancel", (ptrBuiltin)Nelson::HandleGateway::cancelBuiltin, 0, 1 },
    { "enumeration", (ptrBuiltin)Nelson::HandleGateway::enumerationBuiltin, 1, 1 },
    { "events", (ptrBuiltin)Nelson::HandleGateway::eventsBuiltin, 1, 1 },
    { "metaclass", (ptrBuiltin)Nelson::HandleGateway::metaclassBuiltin, 1, 1 },
    { "__meta_package_fromName__", (ptrBuiltin)Nelson::HandleGateway::metaPackageFromNameBuiltin, 1,
        1 },
    { "__meta_package_getAllPackages__",
        (ptrBuiltin)Nelson::HandleGateway::metaPackageGetAllPackagesBuiltin, 1, 0 },
    { "superclasses", (ptrBuiltin)Nelson::HandleGateway::superclassesBuiltin, 1, 1 },
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "vertcat"),
        (ptrBuiltin)Nelson::HandleGateway::handle_vertcatBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "horzcat"),
        (ptrBuiltin)Nelson::HandleGateway::handle_horzcatBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "eq"),
        (ptrBuiltin)Nelson::HandleGateway::handle_eqBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequal"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequaln"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isequalto"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "get"),
        (ptrBuiltin)Nelson::HandleGateway::handle_getBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "set"),
        (ptrBuiltin)Nelson::HandleGateway::handle_setBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "invoke"),
        (ptrBuiltin)Nelson::HandleGateway::handle_invokeBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::HandleGateway::handle_displayBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::HandleGateway::handle_displayBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "delete"),
        (ptrBuiltin)Nelson::HandleGateway::handle_deleteBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isvalid"),
        (ptrBuiltin)Nelson::HandleGateway::handle_isvalidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "fieldnames"),
        (ptrBuiltin)Nelson::HandleGateway::handle_fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "methods"),
        (ptrBuiltin)Nelson::HandleGateway::handle_methodsBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "ismethod"),
        (ptrBuiltin)Nelson::HandleGateway::handle_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "isprop"),
        (ptrBuiltin)Nelson::HandleGateway::handle_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_STR, "properties"),
        (ptrBuiltin)Nelson::HandleGateway::handle_propertiesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("dynamicprops", "addprop"),
        (ptrBuiltin)Nelson::HandleGateway::classdefAddDynamicPropertyBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("event.listener", "delete"),
        (ptrBuiltin)Nelson::HandleGateway::classdefListenerDeleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("event.listener", "isvalid"),
        (ptrBuiltin)Nelson::HandleGateway::classdefListenerIsValidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("meta.DynamicProperty", "delete"),
        (ptrBuiltin)Nelson::HandleGateway::classdefDynamicPropertyDeleteBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("meta.DynamicProperty", "get"),
        (ptrBuiltin)Nelson::HandleGateway::classdefDynamicPropertyGetBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("meta.DynamicProperty", "set"),
        (ptrBuiltin)Nelson::HandleGateway::classdefDynamicPropertySetBuiltin, 0, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("meta.DynamicProperty", "isvalid"),
        (ptrBuiltin)Nelson::HandleGateway::classdefDynamicPropertyIsValidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
};
//=============================================================================
static std::string
externalRootName()
{
    return "nelson";
}
//=============================================================================
static bool
initializeHandleModule(Evaluator*)
{
    classdefRegisterBuiltinClass(
        "dynamicprops", {}, { "addprop" }, {}, false, { "PropertyAdded", "PropertyRemoved" });
    classdefRegisterBuiltinClass(NLS_MIXIN_COPYABLE, {}, { "copy" }, {}, false, {}, { "handle" });
    classdefRegisterBuiltinClass(NLS_MIXIN_CUSTOM_DISPLAY, {}, {}, {});
    classdefRegisterBuiltinClass(
        NLS_MIXIN_SET_GET, {}, { "set", "get" }, {}, false, {}, { "handle" });
    classdefRegisterBuiltinClass(
        NLS_MIXIN_SET_GET_EXACT_NAMES, {}, { "set", "get" }, {}, false, {}, { NLS_MIXIN_SET_GET });
    classdefRegisterBuiltinClass(CLASSDEF_SYSTEM_OBJECT_CLASS, {}, {}, {}, false, {}, { "handle" });
    classdefRegisterBuiltinClass(NLS_MIXIN_HETEROGENEOUS, {}, {}, {});
    classdefRegisterBuiltinClass(NLS_MIXIN_SCALAR, {}, {}, {});
    classdefRegisterBuiltinClass(NLS_MIXIN_CUSTOM_COMPACT_DISPLAY_PROVIDER, {}, {}, {});
    classdefRegisterBuiltinClass(NLS_MIXIN_INDEXING_REDEFINES_PAREN, {}, {}, {});
    classdefRegisterBuiltinClass(NLS_MIXIN_INDEXING_REDEFINES_BRACE, {}, {}, {});
    classdefRegisterBuiltinClass(NLS_MIXIN_INDEXING_REDEFINES_DOT, {}, {}, {});
    classdefRegisterBuiltinClass(
        externalRootName() + ".mixin.indexing.OverridesPublicDotMethodCall", {}, {}, {});
    classdefRegisterBuiltinClass(
        externalRootName() + ".mixin.indexing.ForbidsPublicDotMethodCall", {}, {}, {});
    classdefRegisterBuiltinClass("event.EventData", { "Source", "EventName" }, {}, {}, true);
    classdefRegisterBuiltinClass("event.PropertyEvent",
        { "Source", "EventName", "PropertyName", "AffectedObject" }, {}, {}, true, {},
        { "event.EventData" });
    classdefRegisterBuiltinClass("event.DynamicPropertyEvent",
        { "Source", "EventName", "PropertyName" }, {}, {}, true, {}, { "event.EventData" });
    classdefRegisterBuiltinClass("meta.class",
        { "Name", "SuperclassList", "PropertyList", "MethodList", "EventList",
            "EnumerationMemberList", "Abstract", "Sealed", "Hidden", "ConstructOnLoad", "Handle",
            "InferiorClasses", "ClassDetails", "PropertyDetails", "MethodDetails", "EventDetails",
            "EnumerationDetails" },
        {}, {}, true);
    classdefRegisterBuiltinClass("meta.property",
        { "Name", "DefiningClass", "DefaultValueExpression", "Access", "GetAccess", "SetAccess",
            "GetMethod", "SetMethod", "Constant", "Dependent", "Abstract", "Hidden",
            "GetObservable", "SetObservable", "AbortSet", "Transient", "NonCopyable", "Nontunable",
            "Dynamic", "ValidationExpression", "Attributes" },
        {}, {}, true);
    classdefRegisterBuiltinClass("meta.method",
        { "Name", "DefiningClass", "Access", "Static", "Hidden", "Sealed", "Abstract",
            "Attributes" },
        {}, {}, true);
    classdefRegisterBuiltinClass("meta.event",
        { "Name", "DefiningClass", "ListenAccess", "NotifyAccess", "Hidden", "Attributes" }, {}, {},
        true);
    classdefRegisterBuiltinClass("meta.EnumerationMember",
        { "Name", "DefiningClass", "ConstructorArguments" }, {}, {}, true);
    classdefRegisterBuiltinClass("meta.package",
        { "Name", "FunctionList", "ClassList", "PackageList", "ContainingPackage" }, {}, {}, true);
    // metaclass() returns nelson.metadata.* objects. The legacy meta.* classes are
    // their superclasses so isa(m, 'meta.class') stays true. Properties are inherited.
    classdefRegisterBuiltinClass(NLS_METADATA_CLASS, {}, {}, {}, true, {}, { NLS_META_CLASS });
    classdefRegisterBuiltinClass(
        NLS_METADATA_PROPERTY, {}, {}, {}, true, {}, { NLS_META_PROPERTY });
    classdefRegisterBuiltinClass(NLS_METADATA_METHOD, {}, {}, {}, true, {}, { NLS_META_METHOD });
    classdefRegisterBuiltinClass(NLS_METADATA_EVENT, {}, {}, {}, true, {}, { NLS_META_EVENT });
    classdefRegisterBuiltinClass(
        NLS_METADATA_ENUMERATED_VALUE, {}, {}, {}, true, {}, { NLS_META_ENUMERATION_MEMBER });
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeHandleModule)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
