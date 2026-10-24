//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "quitBuiltin.hpp"
#include "bannerBuiltin.hpp"
#include "evalBuiltin.hpp"
#include "evalcBuiltin.hpp"
#include "evalinBuiltin.hpp"
#include "execstrBuiltin.hpp"
#include "maxNumCompThreadsBuiltin.hpp"
#include "namelengthmaxBuiltin.hpp"
#include "narginBuiltin.hpp"
#include "nargoutBuiltin.hpp"
#include "narginchkBuiltin.hpp"
#include "nargoutchkBuiltin.hpp"
#include "nelsonrootBuiltin.hpp"
#include "nfilenameBuiltin.hpp"
#include "pauseBuiltin.hpp"
#include "prefdirBuiltin.hpp"
#include "runBuiltin.hpp"
#include "versionBuiltin.hpp"
#include "sha256Builtin.hpp"
#include "ed25519_verifyBuiltin.hpp"
#include "ed25519_signBuiltin.hpp"
#include "crypto_sha512Builtin.hpp"
#include "crypto_hmacBuiltin.hpp"
#include "crypto_blake2bBuiltin.hpp"
#include "crypto_argon2Builtin.hpp"
#include "crypto_randomBuiltin.hpp"
#include "crypto_x25519Builtins.hpp"
#include "crypto_aeadBuiltins.hpp"
#include "crc32Builtin.hpp"
#include "inputnameBuiltin.hpp"
#include "featureBuiltin.hpp"
#include "nelsonappidBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"core";
//=============================================================================
static const nlsGateway gateway[] = {
    { "exit", (ptrBuiltin)Nelson::CoreGateway::quitBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "quit", (ptrBuiltin)Nelson::CoreGateway::quitBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "run", (ptrBuiltin)Nelson::CoreGateway::runBuiltin, 1, 3, CPP_BUILTIN_WITH_EVALUATOR },
    { "nfilename", (ptrBuiltin)Nelson::CoreGateway::nfilenameBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mfilename", (ptrBuiltin)Nelson::CoreGateway::nfilenameBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "execstr", (ptrBuiltin)Nelson::CoreGateway::execstrBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "eval", (ptrBuiltin)Nelson::CoreGateway::evalBuiltin, -1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "evalc", (ptrBuiltin)Nelson::CoreGateway::evalcBuiltin, -1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "evalin", (ptrBuiltin)Nelson::CoreGateway::evalinBuiltin, -1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "nargin", (ptrBuiltin)Nelson::CoreGateway::narginBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "narginchk", (ptrBuiltin)Nelson::CoreGateway::narginchkBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "nargout", (ptrBuiltin)Nelson::CoreGateway::nargoutBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "nargoutchk", (ptrBuiltin)Nelson::CoreGateway::nargoutchkBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "pause", (ptrBuiltin)Nelson::CoreGateway::pauseBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "namelengthmax", (ptrBuiltin)Nelson::CoreGateway::namelengthmaxBuiltin, 1, 1 },
    { "nelsonroot", (ptrBuiltin)Nelson::CoreGateway::nelsonrootBuiltin, 1, 0 },
    { "matlabroot", (ptrBuiltin)Nelson::CoreGateway::nelsonrootBuiltin, 1, 0 },
    { "version", (ptrBuiltin)Nelson::CoreGateway::versionBuiltin, 2, 1 },
    { "prefdir", (ptrBuiltin)Nelson::CoreGateway::prefdirBuiltin, 1, 0 },
    { "maxNumCompThreads", (ptrBuiltin)Nelson::CoreGateway::maxNumCompThreadsBuiltin, 1, -1 },
    { "banner", (ptrBuiltin)Nelson::CoreGateway::bannerBuiltin, 0, 0 },
    { "sha256", (ptrBuiltin)Nelson::CoreGateway::sha256Builtin, 1, 2 },
    { "crc32", (ptrBuiltin)Nelson::CoreGateway::crc32Builtin, 1, 2 },
    { "crypto.ed25519.verify", (ptrBuiltin)Nelson::CoreGateway::ed25519_verifyBuiltin, 1, 4 },
    { "crypto.ed25519.sign", (ptrBuiltin)Nelson::CoreGateway::ed25519_signBuiltin, 2, 3 },
    { "crypto.sha256", (ptrBuiltin)Nelson::CoreGateway::sha256Builtin, 1, 2 },
    { "crypto.crc32", (ptrBuiltin)Nelson::CoreGateway::crc32Builtin, 1, 2 },
    { "crypto.sha512", (ptrBuiltin)Nelson::CoreGateway::crypto_sha512Builtin, 1, 2 },
    { "crypto.hmac", (ptrBuiltin)Nelson::CoreGateway::crypto_hmacBuiltin, 1, 4 },
    { "crypto.blake2b", (ptrBuiltin)Nelson::CoreGateway::crypto_blake2bBuiltin, 1, 4 },
    { "crypto.argon2", (ptrBuiltin)Nelson::CoreGateway::crypto_argon2Builtin, 1, 16 },
    { "crypto.random", (ptrBuiltin)Nelson::CoreGateway::crypto_randomBuiltin, 1, 2 },
    { "crypto.x25519.public", (ptrBuiltin)Nelson::CoreGateway::crypto_x25519_publicBuiltin, 1, 1 },
    { "crypto.x25519.shared", (ptrBuiltin)Nelson::CoreGateway::crypto_x25519_sharedBuiltin, 1, 2 },
    { "crypto.x25519.keypair", (ptrBuiltin)Nelson::CoreGateway::crypto_x25519_keypairBuiltin, 2,
        0 },
    { "crypto.aead.encrypt", (ptrBuiltin)Nelson::CoreGateway::crypto_aead_encryptBuiltin, 1, 4 },
    { "crypto.aead.decrypt", (ptrBuiltin)Nelson::CoreGateway::crypto_aead_decryptBuiltin, 1, 4 },
    { "inputname", (ptrBuiltin)Nelson::CoreGateway::inputnameBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "feature", (ptrBuiltin)Nelson::CoreGateway::featureBuiltin, 1, 2, CPP_BUILTIN },
    { "nelsonappid", (ptrBuiltin)Nelson::CoreGateway::nelsonappidBuiltin, 1, 0, CPP_BUILTIN },

};
//=============================================================================
static bool
initializeCoreModule(Nelson::Evaluator* eval)
{
    auto* manager = BuiltInFunctionDefManager::getInstance();
    manager->setCallerContextAccess((ptrBuiltin)Nelson::CoreGateway::runBuiltin, true);
    manager->setCallerContextAccess((ptrBuiltin)Nelson::CoreGateway::execstrBuiltin, true);
    manager->setCallerContextAccess((ptrBuiltin)Nelson::CoreGateway::evalBuiltin, true);
    manager->setCallerContextAccess((ptrBuiltin)Nelson::CoreGateway::evalcBuiltin, true);
    manager->setCallerContextAccess((ptrBuiltin)Nelson::CoreGateway::evalinBuiltin, true);
    return true;
}
//=============================================================================
static bool
finishCoreModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeCoreModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishCoreModule)
//=============================================================================
