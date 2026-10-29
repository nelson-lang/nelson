//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "BlockPlugin.h"
#include "DynamicLibrary.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <nlohmann/json.hpp>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <iostream>
#include <map>
#include <mutex>
#include <algorithm>
#include <cctype>
#include <deque>
#include "FieldNames.hpp"
//=============================================================================
namespace fs = std::filesystem;
using json = nlohmann::json;
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    struct BlockRegistry::Impl
    {
        struct Entry
        {
            BlockHandlerFn handler = nullptr;
            std::set<Phase> phases;
            std::string libraryId;
            BlockMetadata meta;
            bool hasExplicitMeta = false;
        };

        mutable std::mutex mutex;
        std::unordered_map<std::string, Entry> entries;
        std::vector<std::string> manifests;
        std::unordered_map<std::string, std::size_t> manifestIndexes;
        std::vector<std::string> searchPaths;
        std::vector<std::string> discoveryMessages;
        std::unordered_map<std::string, void*> loadedLibraries;
        std::unordered_map<std::string, BlockCodegenTemplate> codegen;
    };
    //=============================================================================
    BlockRegistry::BlockRegistry() : impl_(new Impl()) { }
    //=============================================================================
    BlockRegistry::~BlockRegistry() { delete impl_; }
    //=============================================================================
    BlockRegistry&
    BlockRegistry::instance()
    {
        static BlockRegistry inst;
        return inst;
    }
    //=============================================================================
    void
    BlockRegistry::registerHandler(
        const std::string& type, BlockHandlerFn handler, const std::set<Phase>& phases)
    {
        registerHandler(type, handler, phases, std::string {});
    }
    //=============================================================================
    void
    BlockRegistry::registerHandler(const std::string& type, BlockHandlerFn handler,
        const std::set<Phase>& phases, const std::string& libraryId)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        Impl::Entry e;
        e.handler = handler;
        e.phases = phases;
        e.libraryId = libraryId;
        // Default metadata: feedthrough iff the block participates in the ALGEBRAIC phase.
        e.meta.directFeedthrough = phases.count(Phase::ALGEBRAIC) > 0;
        e.hasExplicitMeta = false;
        impl_->entries[type] = e;
    }
    //=============================================================================
    void
    BlockRegistry::registerHandler(const std::string& type, BlockHandlerFn handler,
        const std::set<Phase>& phases, const std::string& libraryId, const BlockMetadata& meta)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        Impl::Entry e;
        e.handler = handler;
        e.phases = phases;
        e.libraryId = libraryId;
        e.meta = meta;
        e.hasExplicitMeta = true;
        impl_->entries[type] = e;
    }
    //=============================================================================
    BlockMetadata
    BlockRegistry::metadataOf(const std::string& type) const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return BlockMetadata {};
        }
        if (it->second.hasExplicitMeta) {
            return it->second.meta;
        }
        // No explicit metadata: synthesize the default (feedthrough from ALGEBRAIC-phase
        // membership). Start from the stored meta so capabilities patched via
        // setDimsMetadata (vector support, dims rule) are kept.
        BlockMetadata m = it->second.meta;
        m.directFeedthrough = it->second.phases.count(Phase::ALGEBRAIC) > 0;
        return m;
    }
    //=============================================================================
    void
    BlockRegistry::setDimsMetadata(
        const std::string& type, bool acceptsVectorInputs, ResolveDimsFn resolveDims)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.acceptsVectorInputs = acceptsVectorInputs;
        it->second.meta.resolveDims = std::move(resolveDims);
    }
    //=============================================================================
    void
    BlockRegistry::setTypeMetadata(const std::string& type, bool allowMixedInputTypes,
        int fixedOutSigType, bool requiresDoubleInputs)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.allowMixedInputTypes = allowMixedInputTypes;
        it->second.meta.fixedOutSigType = fixedOutSigType;
        it->second.meta.requiresDoubleInputs = requiresDoubleInputs;
    }
    //=============================================================================
    void
    BlockRegistry::setExact64Support(const std::string& type, bool supportsExact64)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.supportsExact64 = supportsExact64;
    }
    //=============================================================================
    void
    BlockRegistry::setAlgebraicOutputEveryStep(const std::string& type, bool everyStep)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.algebraicOutputEveryStep = everyStep;
    }
    //=============================================================================
    void
    BlockRegistry::setComplexSupport(
        const std::string& type, bool acceptsComplexInputs, int forcesComplexOut)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.acceptsComplexInputs = acceptsComplexInputs;
        it->second.meta.forcesComplexOut = forcesComplexOut;
    }
    //=============================================================================
    void
    BlockRegistry::setBusMetadata(
        const std::string& type, bool acceptsBusInputs, ResolveBusDimsFn resolveBusDims)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        if (it == impl_->entries.end()) {
            return;
        }
        it->second.meta.acceptsBusInputs = acceptsBusInputs;
        it->second.meta.resolveBusDims = std::move(resolveBusDims);
    }
    //=============================================================================
    BlockHandlerFn
    BlockRegistry::getHandler(const std::string& type) const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        return (it != impl_->entries.end()) ? it->second.handler : nullptr;
    }
    //=============================================================================
    bool
    BlockRegistry::hasPhase(const std::string& type, Phase phase) const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->entries.find(type);
        return (it != impl_->entries.end()) && it->second.phases.count(phase) > 0;
    }
    //=============================================================================
    void
    BlockRegistry::unregisterLibrary(const std::string& libraryId)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        for (auto it = impl_->entries.begin(); it != impl_->entries.end();) {
            if (it->second.libraryId == libraryId) {
                it = impl_->entries.erase(it);
            } else {
                ++it;
            }
        }
        auto hit = impl_->loadedLibraries.find(libraryId);
        if (hit != impl_->loadedLibraries.end()) {
            close_dynamic_library(static_cast<library_handle>(hit->second));
            impl_->loadedLibraries.erase(hit);
        }
    }
    //=============================================================================
    static Phase
    phaseFromString(const std::string& s)
    {
        if (s == "INIT") {
            return Phase::INIT;
        }
        if (s == "OUTPUT") {
            return Phase::OUTPUT;
        }
        if (s == "ALGEBRAIC") {
            return Phase::ALGEBRAIC;
        }
        if (s == "AFTER_STEP") {
            return Phase::AFTER_STEP;
        }
        if (s == "UPDATE") {
            return Phase::UPDATE;
        }
        if (s == "DERIVATIVE") {
            return Phase::DERIVATIVE;
        }
        if (s == "ZERO_CROSSING") {
            return Phase::ZERO_CROSSING;
        }
        if (s == "EVENT") {
            return Phase::EVENT;
        }
        return Phase::OUTPUT;
    }
    //=============================================================================
    // Bridge between the internal C++ codegen args and a plugin's pure-C
    // NfCodegenContext. One lives on the stack for the duration of a single
    // nfEmitBlockCodegen call; its callbacks read either the step args or the
    // state args (whichever pass is active) and the engine owns every returned
    // string through `storage`.
    //=============================================================================
    namespace {
        struct PluginCodegenBridge
        {
            const BlockCodegenArgs* step = nullptr; // set in the step pass
            const BlockCodegenStateArgs* state = nullptr; // set in the state pass
            bool rust = false;
            std::string id;
            const json* block = nullptr;
            const json* params = nullptr;
            const nflow::VarMap* vars = nullptr;
            std::function<std::string(double)> fmt;
            double dt = 0.0;
            std::deque<std::string> storage; // stable backing for returned c-strings

            const char*
            keep(std::string s)
            {
                storage.push_back(std::move(s));
                return storage.back().c_str();
            }
        };
        //=============================================================================
        PluginCodegenBridge*
        self(void* p)
        {
            return static_cast<PluginCodegenBridge*>(p);
        }
        //=============================================================================
        void
        cb_line(void* s, const char* text)
        {
            PluginCodegenBridge* b = self(s);
            if (b->step && b->step->line && text) {
                b->step->line(text);
            }
        }
        //=============================================================================
        void
        cb_addState(void* s, const char* name, const char* init, const char* type)
        {
            PluginCodegenBridge* b = self(s);
            if (b->state && b->state->addState && name) {
                b->state->addState(std::string(name) + "_" + b->id, init ? init : "",
                    type ? type : "");
            }
        }
        //=============================================================================
        double
        cb_paramNumber(void* s, const char* key, double fallback)
        {
            PluginCodegenBridge* b = self(s);
            if (!key || !b->block || !b->vars) {
                return fallback;
            }
            nflow::BlockDescriptor bd(*b->block, *b->vars);
            return bd.paramDouble(key, fallback);
        }
        //=============================================================================
        const char*
        cb_paramString(void* s, const char* key, const char* fallback)
        {
            PluginCodegenBridge* b = self(s);
            if (!key || !b->block || !b->vars) {
                return fallback;
            }
            nflow::BlockDescriptor bd(*b->block, *b->vars);
            return b->keep(bd.paramStr(key, fallback ? fallback : ""));
        }
        //=============================================================================
        int
        cb_paramBool(void* s, const char* key, int fallback)
        {
            PluginCodegenBridge* b = self(s);
            if (!key || !b->params) {
                return fallback;
            }
            auto it = b->params->find(key);
            if (it == b->params->end() || it->is_null()) {
                return fallback;
            }
            if (it->is_boolean()) {
                return it->get<bool>() ? 1 : 0;
            }
            if (it->is_number()) {
                return it->get<double>() != 0.0 ? 1 : 0;
            }
            if (it->is_string()) {
                const std::string v = it->get<std::string>();
                return (v == "true" || v == "1") ? 1 : 0;
            }
            return fallback;
        }
        //=============================================================================
        int
        cb_hasParam(void* s, const char* key)
        {
            PluginCodegenBridge* b = self(s);
            if (!key || !b->params) {
                return 0;
            }
            auto it = b->params->find(key);
            return (it != b->params->end() && !it->is_null()) ? 1 : 0;
        }
        //=============================================================================
        int
        cb_paramList(void* s, const char* key, double* out, int maxOut)
        {
            PluginCodegenBridge* b = self(s);
            if (!key || !b->block || !b->vars) {
                return 0;
            }
            nflow::BlockDescriptor bd(*b->block, *b->vars);
            std::vector<double> values = bd.paramList(key);
            const int total = static_cast<int>(values.size());
            if (out && maxOut > 0) {
                const int n = total < maxOut ? total : maxOut;
                for (int i = 0; i < n; ++i) {
                    out[i] = values[i];
                }
            }
            return total;
        }
        //=============================================================================
        const char*
        cb_inputExpr(void* s, int port)
        {
            PluginCodegenBridge* b = self(s);
            if (b->step && port >= 0 && port < static_cast<int>(b->step->in.size())) {
                return b->keep(b->step->in[port]);
            }
            return "";
        }
        //=============================================================================
        const char*
        cb_blockId(void* s)
        {
            return self(s)->keep(self(s)->id);
        }
        //=============================================================================
        const char*
        cb_outputVar(void* s)
        {
            PluginCodegenBridge* b = self(s);
            return b->keep("out_" + b->id);
        }
        //=============================================================================
        const char*
        cb_stateRef(void* s, const char* name)
        {
            PluginCodegenBridge* b = self(s);
            const std::string prefix = b->rust ? "s." : "s->";
            return b->keep(prefix + (name ? name : "") + "_" + b->id);
        }
        //=============================================================================
        const char*
        cb_fmt(void* s, double v)
        {
            PluginCodegenBridge* b = self(s);
            if (b->fmt) {
                return b->keep(b->fmt(v));
            }
            return b->keep(std::to_string(v));
        }
        //=============================================================================
        double
        cb_dt(void* s)
        {
            return self(s)->dt;
        }
        //=============================================================================
        const char*
        cb_outType(void* s)
        {
            PluginCodegenBridge* b = self(s);
            if (b->step) {
                return b->keep(b->step->outType);
            }
            return "";
        }
        //=============================================================================
        const char*
        cb_inType(void* s, int port)
        {
            PluginCodegenBridge* b = self(s);
            if (b->step && port >= 0 && port < static_cast<int>(b->step->inTypes.size())) {
                return b->keep(b->step->inTypes[port]);
            }
            return "";
        }
        //=============================================================================
        void
        fillContext(NfCodegenContext& ctx, PluginCodegenBridge* b)
        {
            ctx.self = b;
            ctx.phase = NF_CODEGEN_PHASE_STEP;
            ctx.line = cb_line;
            ctx.addState = cb_addState;
            ctx.paramNumber = cb_paramNumber;
            ctx.paramString = cb_paramString;
            ctx.paramBool = cb_paramBool;
            ctx.hasParam = cb_hasParam;
            ctx.paramList = cb_paramList;
            ctx.inputExpr = cb_inputExpr;
            ctx.blockId = cb_blockId;
            ctx.outputVar = cb_outputVar;
            ctx.stateRef = cb_stateRef;
            ctx.fmt = cb_fmt;
            ctx.dt = cb_dt;
            ctx.outType = cb_outType;
            ctx.inType = cb_inType;
        }
    } // namespace
    //=============================================================================
    bool
    BlockRegistry::loadNativeLibrary(const std::string& path, const std::string& libraryId)
    {
        {
            std::lock_guard<std::mutex> lk(impl_->mutex);
            if (impl_->loadedLibraries.count(libraryId) > 0) {
                return true;
            }
        }

        // Convert through fs::path so the platform narrow encoding is honored
        // (a raw byte-widening copy corrupts any non-ASCII path).
        std::wstring wpath = fs::path(path).wstring();
        library_handle handle = load_dynamic_libraryW(wpath);
        if (!handle) {
            std::cerr << "[BlockRegistry] Failed to load native library: " << path << "\n";
            std::cerr << "  Error: " << get_dynamic_library_error() << "\n";
            return false;
        }

        // ABI handshake: the plugin reads SimCtx/Block structs directly, so a
        // layout mismatch is memory corruption, not a soft failure. Refuse any
        // plugin that does not report the exact ABI version it was built for.
        auto getAbiVersionFn
            = reinterpret_cast<NfGetAbiVersionFn>(get_function(handle, NF_GET_ABI_VERSION_SYMBOL));
        const int pluginAbi = getAbiVersionFn ? getAbiVersionFn() : 0;
        if (pluginAbi != NF_PLUGIN_ABI_VERSION) {
            std::cerr << "[BlockRegistry] Rejecting plugin '" << path << "': ABI version "
                      << pluginAbi << " (0 = nfGetAbiVersion missing), engine requires "
                      << NF_PLUGIN_ABI_VERSION << ". Rebuild the plugin against the current "
                      << "SimEngineTypes.hpp / BlockPlugin.h.\n";
            close_dynamic_library(handle);
            return false;
        }

        auto getHandlersFn = reinterpret_cast<NfGetBlockHandlersFn>(
            get_function(handle, NF_GET_BLOCK_HANDLERS_SYMBOL));
        if (!getHandlersFn) {
            std::cerr << "[BlockRegistry] Symbol not found: " << NF_GET_BLOCK_HANDLERS_SYMBOL
                      << "\n";
            close_dynamic_library(handle);
            return false;
        }

        int count = 0;
        const NfBlockHandlerEntry* entries = getHandlersFn(&count);
        if (!entries || count <= 0) {
            close_dynamic_library(handle);
            return false;
        }

        // Optional phase info: returns a JSON string {"type":["INIT","OUTPUT",...], ...}
        json phaseMap;
        auto getPhaseInfoFn
            = reinterpret_cast<NfGetPhaseInfoFn>(get_function(handle, NF_GET_PHASE_INFO_SYMBOL));
        if (getPhaseInfoFn) {
            const char* phaseJson = getPhaseInfoFn();
            if (phaseJson) {
                try {
                    phaseMap = json::parse(phaseJson);
                } catch (...) {
                }
            }
        }

        {
            std::lock_guard<std::mutex> lk(impl_->mutex);
            impl_->loadedLibraries[libraryId] = static_cast<void*>(handle);
        }

        for (int i = 0; i < count; ++i) {
            const NfBlockHandlerEntry& e = entries[i];
            if (!e.type || !e.handler) {
                continue;
            }

            std::set<Phase> phases;
            std::string blockType(e.type);
            if (phaseMap.contains(blockType) && phaseMap[blockType].is_array()) {
                for (auto& pv : phaseMap[blockType]) {
                    if (pv.is_string()) {
                        phases.insert(phaseFromString(pv.get<std::string>()));
                    }
                }
            }
            if (phases.empty()) {
                phases.insert(Phase::OUTPUT);
            }

            auto cHandler = e.handler;
            BlockHandlerFn wrapped
                = [cHandler](SimCtx& ctx, const Block& block, Phase phase) -> bool {
                return cHandler(static_cast<void*>(&ctx), static_cast<const void*>(&block),
                           static_cast<int>(phase))
                    != 0;
            };
            registerHandler(blockType, wrapped, phases, libraryId);
        }

        // Optional programmatic codegen: nfEmitBlockCodegen(type, lang, ctx)
        // emits code through callbacks. Detected per (type, lang) by a dry-run
        // probe (a detached context whose sinks are no-ops): return 1 means the
        // block emits for that language. Registered first so it takes precedence
        // over any JSON template the plugin also exports for the same pair.
        std::set<std::string> programmatic;
        auto emitCodegenFn = reinterpret_cast<NfEmitBlockCodegenFn>(
            get_function(handle, NF_EMIT_BLOCK_CODEGEN_SYMBOL));
        if (emitCodegenFn) {
            for (int i = 0; i < count; ++i) {
                const NfBlockHandlerEntry& e = entries[i];
                if (!e.type) {
                    continue;
                }
                for (const char* lang : { "c", "rust" }) {
                    PluginCodegenBridge probe; // detached: all sinks no-op
                    NfCodegenContext ctx;
                    fillContext(ctx, &probe);
                    if (emitCodegenFn(e.type, lang, &ctx) == 1) {
                        registerCodegenEmitter(e.type, lang, emitCodegenFn);
                        programmatic.insert(std::string(e.type) + "\n" + lang);
                    }
                }
            }
        }

        // Optional codegen templates: nfGetBlockCodegen(type, lang) returns a JSON
        // template ({"state":[{"name":...,"init":...}],"step":"...","inputDefault":"..."})
        // or NULL when the block has no codegen for that language.
        auto getCodegenFn = reinterpret_cast<NfGetBlockCodegenFn>(
            get_function(handle, NF_GET_BLOCK_CODEGEN_SYMBOL));
        if (getCodegenFn) {
            for (int i = 0; i < count; ++i) {
                const NfBlockHandlerEntry& e = entries[i];
                if (!e.type) {
                    continue;
                }
                for (const char* lang : { "c", "rust" }) {
                    // The programmatic emitter wins when both are present.
                    if (programmatic.count(std::string(e.type) + "\n" + lang) > 0) {
                        continue;
                    }
                    const char* tplJson = getCodegenFn(e.type, lang);
                    if (!tplJson) {
                        continue;
                    }
                    try {
                        json j = json::parse(tplJson);
                        BlockCodegenTemplate tpl;
                        if (j.contains("step") && j["step"].is_string()) {
                            tpl.step = j["step"].get<std::string>();
                        }
                        if (j.contains("inputDefault") && j["inputDefault"].is_string()) {
                            tpl.inputDefault = j["inputDefault"].get<std::string>();
                        }
                        if (j.contains("state") && j["state"].is_array()) {
                            for (auto& sv : j["state"]) {
                                BlockCodegenTemplate::StateVar v;
                                if (sv.contains("name") && sv["name"].is_string()) {
                                    v.name = sv["name"].get<std::string>();
                                }
                                if (sv.contains("init") && sv["init"].is_string()) {
                                    v.init = sv["init"].get<std::string>();
                                }
                                if (!v.name.empty()) {
                                    tpl.state.push_back(v);
                                }
                            }
                        }
                        if (!tpl.step.empty() || !tpl.state.empty()) {
                            registerCodegen(e.type, lang, tpl);
                        }
                    } catch (...) {
                        std::cerr << "[BlockRegistry] Invalid codegen template for type '" << e.type
                                  << "' (" << lang << ")\n";
                    }
                }
            }
        }
        return true;
    }
    //=============================================================================
    void
    BlockRegistry::registerCodegenEmitter(
        const std::string& type, const std::string& lang, NfEmitBlockCodegenFn fn)
    {
        if (!fn) {
            return;
        }
        const bool rust = (lang == "rust");
        BlockCodegenTemplate tpl;
        tpl.emitStep = [fn, type, lang, rust](const BlockCodegenArgs& a) {
            PluginCodegenBridge b;
            b.step = &a;
            b.rust = rust;
            b.id = a.id;
            b.block = a.block;
            b.params = a.params;
            b.vars = a.variables;
            b.fmt = a.fmt;
            b.dt = a.dt;
            NfCodegenContext ctx;
            fillContext(ctx, &b);
            ctx.phase = NF_CODEGEN_PHASE_STEP;
            fn(type.c_str(), lang.c_str(), &ctx);
        };
        tpl.emitState = [fn, type, lang, rust](const BlockCodegenStateArgs& sa) {
            PluginCodegenBridge b;
            b.state = &sa;
            b.rust = rust;
            b.id = sa.id;
            b.block = sa.block;
            b.params = sa.params;
            b.vars = sa.variables;
            b.fmt = sa.fmt;
            b.dt = sa.dt;
            NfCodegenContext ctx;
            fillContext(ctx, &b);
            ctx.phase = NF_CODEGEN_PHASE_STATE;
            fn(type.c_str(), lang.c_str(), &ctx);
        };
        registerCodegen(type, lang, tpl);
    }
    //=============================================================================
    void
    BlockRegistry::registerCodegen(
        const std::string& type, const std::string& lang, const BlockCodegenTemplate& tmpl)
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        impl_->codegen[type + "\n" + lang] = tmpl;
    }
    //=============================================================================
    void
    BlockRegistry::registerCodegen(const std::string& type, const BlockCodegenPair& pair)
    {
        registerCodegen(type, "c", pair.c);
        registerCodegen(type, "rust", pair.rust);
    }
    //=============================================================================
    std::vector<std::string>
    BlockRegistry::codegenLanguageAsymmetries() const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        std::map<std::string, int> langsPerType;
        for (const auto& kv : impl_->codegen) {
            const size_t sep = kv.first.find('\n');
            if (sep == std::string::npos) {
                continue;
            }
            langsPerType[kv.first.substr(0, sep)]++;
        }
        std::vector<std::string> asymmetric;
        for (const auto& kv : langsPerType) {
            if (kv.second == 1) {
                asymmetric.push_back(kv.first);
            }
        }
        return asymmetric;
    }
    //=============================================================================
    bool
    BlockRegistry::getCodegen(
        const std::string& type, const std::string& lang, BlockCodegenTemplate& out) const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        auto it = impl_->codegen.find(type + "\n" + lang);
        if (it == impl_->codegen.end()) {
            return false;
        }
        out = it->second;
        return true;
    }
    //=============================================================================
    bool
    BlockRegistry::scanDirectory(const std::string& dirPath)
    {
        std::error_code ec;
        if (!fs::is_directory(fs::path(dirPath), ec)) {
            return false;
        }

        bool found = false;
        fs::directory_iterator iterator(
            fs::path(dirPath), fs::directory_options::skip_permission_denied, ec);
        const fs::directory_iterator end;
        for (; !ec && iterator != end; iterator.increment(ec)) {
            const auto& entry = *iterator;
            std::error_code entryEc;
            if (!entry.is_directory(entryEc)) {
                continue;
            }
            fs::path manifestPath = entry.path() / "library.json";
            if (!fs::exists(manifestPath)) {
                continue;
            }

            std::ifstream ifs(manifestPath);
            if (!ifs.is_open()) {
                continue;
            }

            try {
                json manifest = json::parse(ifs);
                manifest["_dir"] = entry.path().string();
                std::string libId = manifest.value(nflow::kId, entry.path().filename().string());
                if (libId.rfind("builtin.", 0) == 0) {
                    std::lock_guard<std::mutex> lk(impl_->mutex);
                    impl_->discoveryMessages.push_back(
                        "Reserved library identifier ignored: " + libId);
                    continue;
                }

                // The first search path wins. Record duplicates for diagnostics.
                {
                    std::lock_guard<std::mutex> lk(impl_->mutex);
                    auto existing = impl_->manifestIndexes.find(libId);
                    if (existing != impl_->manifestIndexes.end()) {
                        bool sameLocation = false;
                        try {
                            const json known = json::parse(impl_->manifests[existing->second]);
                            sameLocation = fs::path(known.value("_dir", "")).lexically_normal()
                                == entry.path().lexically_normal();
                        } catch (...) {
                        }
                        if (!sameLocation) {
                            impl_->discoveryMessages.push_back("Duplicate library ignored: " + libId
                                + " (" + entry.path().string() + ")");
                            continue;
                        }
                        impl_->manifests[existing->second] = manifest.dump();
                    } else {
                        impl_->manifestIndexes[libId] = impl_->manifests.size();
                        impl_->manifests.push_back(manifest.dump());
                    }
                }

                std::string ext = get_dynamic_library_extension();
                bool loadedNative = false;
                if (manifest.contains("native") && manifest["native"].is_string()) {
                    std::string nativeField = manifest["native"].get<std::string>();
                    if (!nativeField.empty()) {
                        fs::path candidate = entry.path() / nativeField;
                        if (fs::exists(candidate)) {
                            loadNativeLibrary(candidate.string(), libId);
                            loadedNative = true;
                        } else {
                            fs::path mp(nativeField);
                            fs::path base = entry.path();
                            if (mp.has_parent_path()) {
                                base = base / mp.parent_path();
                            }
                            std::string stem = mp.stem().string();
                            fs::path alt = base / (stem + ext);
                            if (fs::exists(alt)) {
                                loadNativeLibrary(alt.string(), libId);
                                loadedNative = true;
                            }
                        }
                    }
                }

                // Fallback: look for a library named "blocks" next to the manifest
                if (!loadedNative) {
                    fs::path nativePath = entry.path() / ("blocks" + ext);
                    if (fs::exists(nativePath)) {
                        loadNativeLibrary(nativePath.string(), libId);
                    }
                }

                found = true;
            } catch (...) {
                std::lock_guard<std::mutex> lk(impl_->mutex);
                impl_->discoveryMessages.push_back(
                    "Invalid library manifest ignored: " + manifestPath.string());
            }
        }
        return found;
    }
    //=============================================================================
    int
    BlockRegistry::scanSearchPaths(const std::vector<std::string>& paths)
    {
        int total = 0;
        for (const auto& p : paths) {
            if (scanDirectory(p)) {
                ++total;
            }
        }
        return total;
    }
    //=============================================================================
    static std::string
    normalizedSearchPath(const std::string& path)
    {
        if (path.empty()) {
            return {};
        }
        std::error_code ec;
        fs::path normalized = fs::absolute(fs::path(path), ec);
        if (ec) {
            normalized = fs::path(path);
        }
        return normalized.lexically_normal().generic_string();
    }
    //=============================================================================
    int
    BlockRegistry::setSearchPaths(const std::vector<std::string>& paths)
    {
        std::vector<std::string> normalized;
        std::set<std::string> seen;
        for (const auto& path : paths) {
            fs::path requested(path);
            std::error_code ec;
            if (fs::exists(requested / "library.json", ec)) {
                requested = requested.parent_path();
            }
            std::string value = normalizedSearchPath(requested.string());
            if (value.empty()) {
                continue;
            }
#ifdef _WIN32
            std::string key = value;
            std::transform(key.begin(), key.end(), key.begin(),
                [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
#else
            const std::string& key = value;
#endif
            if (seen.insert(key).second) {
                normalized.push_back(value);
            }
        }

        std::vector<std::string> loadedIds;
        {
            std::lock_guard<std::mutex> lk(impl_->mutex);
            for (const auto& item : impl_->loadedLibraries) {
                loadedIds.push_back(item.first);
            }
        }
        for (const auto& id : loadedIds) {
            unregisterLibrary(id);
        }
        {
            std::lock_guard<std::mutex> lk(impl_->mutex);
            impl_->manifests.clear();
            impl_->manifestIndexes.clear();
            impl_->discoveryMessages.clear();
            impl_->searchPaths = normalized;
            for (const auto& path : normalized) {
                std::error_code ec;
                if (!fs::is_directory(fs::path(path), ec)) {
                    impl_->discoveryMessages.push_back("Library location unavailable: " + path);
                }
            }
        }
        return scanSearchPaths(normalized);
    }
    //=============================================================================
    std::vector<std::string>
    BlockRegistry::searchPaths() const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        return impl_->searchPaths;
    }
    //=============================================================================
    std::vector<std::string>
    BlockRegistry::registeredTypes() const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        std::vector<std::string> types;
        types.reserve(impl_->entries.size());
        for (const auto& kv : impl_->entries) {
            types.push_back(kv.first);
        }
        return types;
    }
    //=============================================================================
    std::vector<std::string>
    BlockRegistry::discoveredManifests() const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        return impl_->manifests;
    }
    //=============================================================================
    std::vector<std::string>
    BlockRegistry::discoveryMessages() const
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        return impl_->discoveryMessages;
    }
    //=============================================================================
    void
    BlockRegistry::clear()
    {
        std::lock_guard<std::mutex> lk(impl_->mutex);
        impl_->entries.clear();
        impl_->manifests.clear();
        impl_->manifestIndexes.clear();
        impl_->searchPaths.clear();
        impl_->discoveryMessages.clear();
        for (auto& kv : impl_->loadedLibraries) {
            close_dynamic_library(static_cast<library_handle>(kv.second));
        }
        impl_->loadedLibraries.clear();
    }
    //=============================================================================
    void
    registerAllBuiltinHandlers()
    {
        auto& reg = BlockRegistry::instance();
        registerSourceHandlers(reg);
        registerMathHandlers(reg);
        registerLookupHandlers(reg);
        registerNonlinearHandlers(reg);
        registerLogicHandlers(reg);
        registerContinuousHandlers(reg);
        registerDiscreteHandlers(reg);
        registerSinkHandlers(reg);
        registerDashboardHandlers(reg);
        registerSubsystemHandlers(reg);
        registerCondLoweringHandlers(reg);
        registerFmuBlock(reg);
        registerFmuMeBlock(reg);
        registerRoutingHandlers(reg);
        registerMatrixHandlers(reg);
        registerComplexHandlers(reg);
        registerUserDefinedHandlers(reg);
        // Last: patch the vector-signal capabilities onto the entries above.
        registerVectorCapabilities(reg);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
