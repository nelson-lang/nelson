//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "zipBuiltin.hpp"
#include "unzipBuiltin.hpp"
#include "applicationArchiveBuiltin.hpp"
#include "zipOpenBuiltin.hpp"
#include "zipEntriesBuiltin.hpp"
#include "zipReadBuiltin.hpp"
#include "zipWriteBuiltin.hpp"
#include "zipAddBuiltin.hpp"
#include "zipCloseBuiltin.hpp"
#include "zipCompressionsBuiltin.hpp"
#include "zipRewriteBuiltin.hpp"
#include "streamCompressBuiltin.hpp"
#include "streamDecompressBuiltin.hpp"
#include "streamCompressFileBuiltin.hpp"
#include "streamDecompressFileBuiltin.hpp"
#include "streamVersionBuiltin.hpp"
#include "zipFopenBuiltin.hpp"
#include "tarBuiltin.hpp"
#include "untarBuiltin.hpp"
#include "tarOpenBuiltin.hpp"
#include "tarEntriesBuiltin.hpp"
#include "tarReadBuiltin.hpp"
#include "tarWriteBuiltin.hpp"
#include "tarAddBuiltin.hpp"
#include "tarCloseBuiltin.hpp"
#include "tarFopenBuiltin.hpp"
#include "tarExtractBuiltin.hpp"
#include "streamFopenBuiltin.hpp"
#include "streamTobytesBuiltin.hpp"
#include "archiveOpenBuiltin.hpp"
#include "tarCompressionsBuiltin.hpp"
#include "interruptibleBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"file_archiver";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__application_archive__", (ptrBuiltin)Nelson::FileArchiverGateway::applicationArchiveBuiltin,
        0, -6 },
    { "zip",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::zipBuiltin>,
        -1, -3 },
    { "__unzip__",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::unzipBuiltin>,
        -1, -2 },
    { "__zip_open__", (ptrBuiltin)Nelson::FileArchiverGateway::zipOpenBuiltin, 1, 4 },
    { "__zip_entries__", (ptrBuiltin)Nelson::FileArchiverGateway::zipEntriesBuiltin, 1, 1 },
    { "__zip_read__", (ptrBuiltin)Nelson::FileArchiverGateway::zipReadBuiltin, 1, 3 },
    { "__zip_write__", (ptrBuiltin)Nelson::FileArchiverGateway::zipWriteBuiltin, 0, 6 },
    { "__zip_add__",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::zipAddBuiltin>,
        1, 5 },
    { "__zip_close__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::zipCloseBuiltin>,
        1, 2 },
    { "__zip_compressions__", (ptrBuiltin)Nelson::FileArchiverGateway::zipCompressionsBuiltin, 1,
        0 },
    { "__zip_rewrite__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::zipRewriteBuiltin>,
        0, 4 },
    { "__zip_fopen__", (ptrBuiltin)Nelson::FileArchiverGateway::zipFopenBuiltin, 1, 9 },
    { "__stream_compress__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::streamCompressBuiltin>,
        1, 4 },
    { "__stream_decompress__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::streamDecompressBuiltin>,
        1, 3 },
    { "__stream_compressfile__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::streamCompressFileBuiltin>,
        0, 5 },
    { "__stream_decompressfile__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::streamDecompressFileBuiltin>,
        0, 4 },
    { "__stream_version__", (ptrBuiltin)Nelson::FileArchiverGateway::streamVersionBuiltin, 1, 1 },
    { "__stream_fopen__", (ptrBuiltin)Nelson::FileArchiverGateway::streamFopenBuiltin, 1, 8 },
    { "__stream_tobytes__", (ptrBuiltin)Nelson::FileArchiverGateway::streamTobytesBuiltin, 1, 1 },
    { "tar",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::tarBuiltin>,
        -1, -3 },
    { "__untar__",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::untarBuiltin>,
        -1, -2 },
    { "__tar_open__", (ptrBuiltin)Nelson::FileArchiverGateway::tarOpenBuiltin, 1, 4 },
    { "__tar_entries__", (ptrBuiltin)Nelson::FileArchiverGateway::tarEntriesBuiltin, 1, 1 },
    { "__tar_read__", (ptrBuiltin)Nelson::FileArchiverGateway::tarReadBuiltin, 1, 3 },
    { "__tar_write__", (ptrBuiltin)Nelson::FileArchiverGateway::tarWriteBuiltin, 0, 4 },
    { "__tar_add__",
        (ptrBuiltin)
            Nelson::FileArchiverGateway::interruptible<Nelson::FileArchiverGateway::tarAddBuiltin>,
        1, 3 },
    { "__tar_close__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::tarCloseBuiltin>,
        1, 2 },
    { "__tar_fopen__", (ptrBuiltin)Nelson::FileArchiverGateway::tarFopenBuiltin, 1, 6 },
    { "__tar_extract__",
        (ptrBuiltin)Nelson::FileArchiverGateway::interruptible<
            Nelson::FileArchiverGateway::tarExtractBuiltin>,
        1, 2 },
    { "__archive_open__", (ptrBuiltin)Nelson::FileArchiverGateway::archiveOpenBuiltin, 1, 2 },
    { "__tar_compressions__", (ptrBuiltin)Nelson::FileArchiverGateway::tarCompressionsBuiltin, 1,
        0 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
