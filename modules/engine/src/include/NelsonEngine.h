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
#include "nlsEngine_exports.h"
#ifndef __cplusplus
#include <stdbool.h>
#endif
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    struct nelson_engine_tag;

    /**
     * Opaque handle for a Nelson engine connection.
     *
     * Handles returned by nelson_engine_start own the child Nelson process and close
     * it when nelson_engine_close is called. Handles returned by
     * nelson_engine_connect attach to an existing Nelson process and detach on
     * nelson_engine_close without terminating that process.
     */
    typedef struct nelson_engine_tag NelsonEngineHandle;
    //=============================================================================
    /**
     * Start a new Nelson engine process.
     *
     * @param option command-line options passed to Nelson. If null or empty, the
     *        implementation uses the default minimized startup option.
     * @param errorMessage receives an allocated UTF-8 error string on failure. The
     *        caller must release it with nelson_engine_free_string.
     * @return an owned engine handle on success, or null on failure.
     *
     * The launcher resolves Nelson from the current process environment when
     * possible. In particular, NELSON_ROOT can be used by clients such as the Python
     * package to start Nelson from outside the source or installation directory.
     */
    NLSENGINE_IMPEXP NelsonEngineHandle*
    nelson_engine_start(const char* option, char** errorMessage);
    //=============================================================================
    /**
     * Connect to an existing Nelson engine process.
     *
     * @param pid target Nelson process id. If pid is less than or equal to zero, the
     *        latest discoverable Nelson process is used.
     * @param errorMessage receives an allocated UTF-8 error string on failure. The
     *        caller must release it with nelson_engine_free_string.
     * @return a non-owning engine handle on success, or null on failure.
     */
    NLSENGINE_IMPEXP NelsonEngineHandle*
    nelson_engine_connect(int pid, char** errorMessage);
    //=============================================================================
    /**
     * Find discoverable Nelson engine process ids.
     *
     * @param pids optional caller-provided buffer that receives process ids.
     * @param capacity number of entries available in pids.
     * @return total number of discoverable Nelson processes, which can be larger
     *         than capacity.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_find(int* pids, int capacity);
    //=============================================================================
    /**
     * Evaluate a Nelson command in the engine process.
     *
     * @param engine valid engine handle.
     * @param command UTF-8 Nelson command text.
     * @param output receives allocated UTF-8 output text. The caller must release it
     *        with nelson_engine_free_string.
     * @param errorMessage receives allocated UTF-8 error text when the command or
     *        IPC call fails. The caller must release it with
     *        nelson_engine_free_string.
     * @return zero on success, non-zero on failure.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_eval(
        NelsonEngineHandle* engine, const char* command, char** output, char** errorMessage);
    //=============================================================================
    /**
     * Read a Nelson workspace variable as JSON.
     *
     * @param engine valid engine handle.
     * @param name UTF-8 workspace variable name.
     * @param json receives an allocated UTF-8 JSON document. The caller must release
     *        it with nelson_engine_free_string.
     * @param errorMessage receives allocated UTF-8 error text on failure. The caller
     *        must release it with nelson_engine_free_string.
     * @return zero on success, non-zero on failure.
     *
     * JSON conversion is intentionally limited to stable ArrayOf conversions. Python
     * compatibility code reads structs, tables, sparse arrays, dictionaries, and
     * object handles through Nelson-side helper commands when direct native
     * serialization would be lossy or unsafe.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_get_variable_json(
        NelsonEngineHandle* engine, const char* name, char** json, char** errorMessage);
    //=============================================================================
    /**
     * Write a Nelson workspace variable from JSON.
     *
     * @param engine valid engine handle.
     * @param name UTF-8 workspace variable name.
     * @param json UTF-8 JSON document using the engine transfer schema.
     * @param errorMessage receives allocated UTF-8 error text on failure. The caller
     *        must release it with nelson_engine_free_string.
     * @return zero on success, non-zero on failure.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_put_variable_json(
        NelsonEngineHandle* engine, const char* name, const char* json, char** errorMessage);
    //=============================================================================
    /**
     * Set GUI visibility for the connected Nelson process when supported.
     *
     * @return zero on success, non-zero on failure.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_set_visible(NelsonEngineHandle* engine, bool visible, char** errorMessage);
    //=============================================================================
    /**
     * Query GUI visibility for the connected Nelson process when supported.
     *
     * @param visible receives the visibility flag on success.
     * @return zero on success, non-zero on failure.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_get_visible(NelsonEngineHandle* engine, bool* visible, char** errorMessage);
    //=============================================================================
    /**
     * Close or detach an engine handle.
     *
     * Owned handles terminate the child Nelson process. Connected handles only detach
     * from the existing process. After this call, the handle must not be reused.
     *
     * @return zero on success, non-zero on failure.
     */
    NLSENGINE_IMPEXP int
    nelson_engine_close(NelsonEngineHandle* engine, char** errorMessage);
    //=============================================================================
    /**
     * Release strings allocated by this C API.
     *
     * Use this for output, JSON, and error strings returned through char** output
     * parameters. Passing null is allowed.
     */
    NLSENGINE_IMPEXP void
    nelson_engine_free_string(char* value);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
