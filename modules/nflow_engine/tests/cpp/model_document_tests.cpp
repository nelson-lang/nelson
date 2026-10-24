//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "NFlowBlockCatalog.hpp"
#include "NFlowModelDocument.hpp"
#include "NFlowModelRegistry.hpp"
#include <string>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
// The libraries directory is passed at configure time so the catalog can be
// loaded without the module-path machinery.
#ifndef NFLOW_LIBRARIES_DIR
#define NFLOW_LIBRARIES_DIR ""
#endif
static void
loadCatalog()
{
    NFlowBlockCatalog::instance().clear();
    std::string dir = NFLOW_LIBRARIES_DIR;
    std::wstring wdir(dir.begin(), dir.end());
    NFlowBlockCatalog::instance().loadManifestDirectory(wdir);
}
//=============================================================================
TEST_CASE("catalog resolves the built-in block types and source forms")
{
    loadCatalog();
    auto& cat = NFlowBlockCatalog::instance();
    CHECK(cat.hasType("gain"));
    CHECK(cat.hasType("sine"));
    CHECK(cat.hasType("scope"));

    std::string t;
    bool amb = false;
    CHECK(cat.resolveType("gain", t, amb));
    CHECK(t == "gain");
    CHECK(cat.resolveType("nflow/math/gain", t, amb));
    CHECK(t == "gain");
    CHECK(cat.resolveType("nflow.builtin.math.gain", t, amb));
    CHECK(t == "gain");
    CHECK_FALSE(cat.resolveType("does_not_exist", t, amb));

    const BlockCatalogEntry* gain = cat.find("gain");
    REQUIRE(gain != nullptr);
    CHECK(gain->inputCount == 1);
    CHECK(gain->outputCount == 1);
    CHECK(gain->defaultParams.contains("Gain"));
}
//=============================================================================
TEST_CASE("document builds the target diagram and round-trips")
{
    loadCatalog();
    NFlowModelDocument doc("demo");
    std::string name, err;

    REQUIRE(doc.addBlock("nflow/source/sine", "Sine", false, name, err));
    CHECK(name == "Sine");
    REQUIRE(doc.addBlock("nflow/math/gain", "Gain", false, name, err));
    REQUIRE(doc.addBlock("nflow/sink/scope", "Scope", false, name, err));

    REQUIRE(doc.setBlockParam("Gain", "Gain", "2", err));
    std::string v;
    REQUIRE(doc.getBlockParam("Gain", "Gain", v, err));
    CHECK(v == "2");

    REQUIRE(doc.addLine("Sine", 1, "Gain", 1, true, err));
    REQUIRE(doc.addLine("Gain", 1, "Scope", 1, true, err));

    REQUIRE(doc.setModelParam("StopTime", "10", err));
    REQUIRE(doc.getModelParam("StopTime", v, err));
    CHECK(v == "10");

    std::string text = doc.toJsonString();
    NFlowModelDocument copy("demo");
    REQUIRE(NFlowModelDocument::fromJsonString(text, "demo", copy, err));
    CHECK(copy.hasBlock("Sine"));
    CHECK(copy.hasBlock("Gain"));
    CHECK(copy.hasBlock("Scope"));
    REQUIRE(copy.getBlockParam("Gain", "Gain", v, err));
    CHECK(v == "2");
}
//=============================================================================
TEST_CASE("document validates connections and rename rewires")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;
    doc.addBlock("gain", "A", false, name, err);
    doc.addBlock("gain", "B", false, name, err);
    doc.addBlock("gain", "C", false, name, err);

    REQUIRE(doc.addLine("A", 1, "B", 1, true, err));
    // Input already occupied.
    CHECK_FALSE(doc.addLine("C", 1, "B", 1, true, err));
    // Duplicate.
    CHECK_FALSE(doc.addLine("A", 1, "B", 1, true, err));
    // Fan-out from a single output is allowed.
    REQUIRE(doc.addLine("A", 1, "C", 1, true, err));

    REQUIRE(doc.renameBlock("A", "Source", err));
    CHECK_FALSE(doc.hasBlock("A"));
    CHECK(doc.hasBlock("Source"));
    const auto& conns = doc.diagram()["connections"];
    bool rewired = false;
    for (const auto& c : conns) {
        if (c.value("from", std::string()) == "Source") {
            rewired = true;
        }
    }
    CHECK(rewired);

    // Delete cascades to connections.
    REQUIRE(doc.deleteBlock("Source", err));
    CHECK(doc.diagram()["connections"].empty());
}
//=============================================================================
TEST_CASE("MakeNameUnique and connectBlocks")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;
    REQUIRE(doc.addBlock("gain", "Gain", false, name, err));
    CHECK(name == "Gain");
    REQUIRE(doc.addBlock("gain", "Gain", true, name, err));
    CHECK(name == "Gain1");
    CHECK_FALSE(doc.addBlock("gain", "Gain", false, name, err));

    doc.addBlock("sine", "Src", false, name, err);
    REQUIRE(doc.connectBlocks("Src", "Gain", err));
    // Second connectBlocks to the same one-input block fails (no free input).
    doc.addBlock("sine", "Src2", false, name, err);
    CHECK_FALSE(doc.connectBlocks("Src2", "Gain", err));
}
//=============================================================================
TEST_CASE("document tracks revision and change origin; replaceDiagram is atomic")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;

    // Fresh document: no revision yet, origin is Load, not dirty.
    CHECK(doc.revision() == 0);
    CHECK(doc.lastOrigin() == ChangeOrigin::Load);
    CHECK_FALSE(doc.dirty());

    // Each scripted mutation bumps the revision and records the Script origin.
    REQUIRE(doc.addBlock("gain", "G", false, name, err));
    std::uint64_t r1 = doc.revision();
    CHECK(r1 > 0);
    CHECK(doc.lastOrigin() == ChangeOrigin::Script);
    CHECK(doc.dirty());

    REQUIRE(doc.setBlockParam("G", "Gain", "3", err));
    std::uint64_t r2 = doc.revision();
    CHECK(r2 > r1);

    // Build an edited diagram (as the GUI would): current snapshot + one block,
    // with a different payload name to prove identity is document-owned.
    NFlowModelDocument builder("m");
    REQUIRE(NFlowModelDocument::fromJsonString(doc.toJsonString(), "m", builder, err));
    REQUIRE(builder.addBlock("sine", "S", false, name, err));
    nlohmann::json edited = builder.diagram();
    edited["name"] = "payload_name_should_be_ignored";

    // A GUI commit replaces the whole diagram, records the Gui origin, keeps the
    // document identity and bumps the revision.
    REQUIRE(doc.replaceDiagram(edited, ChangeOrigin::Gui, err));
    CHECK(doc.revision() > r2);
    CHECK(doc.lastOrigin() == ChangeOrigin::Gui);
    CHECK(doc.hasBlock("G"));
    CHECK(doc.hasBlock("S"));
    CHECK(doc.name() == "m");
    std::string nm;
    REQUIRE(doc.getModelParam("Name", nm, err));
    CHECK(nm == "m");

    // A malformed payload is rejected without touching the current diagram.
    std::uint64_t rBefore = doc.revision();
    CHECK_FALSE(doc.replaceDiagram(nlohmann::json::array(), ChangeOrigin::Gui, err));
    CHECK(doc.revision() == rBefore);
    CHECK(doc.hasBlock("S"));
}
//=============================================================================
TEST_CASE("document change listener fires on every mutation with origin")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;

    int calls = 0;
    std::uint64_t lastRev = 0;
    ChangeOrigin lastOrigin = ChangeOrigin::Load;
    doc.setChangeListener([&](std::uint64_t rev, ChangeOrigin origin) {
        ++calls;
        lastRev = rev;
        lastOrigin = origin;
    });

    REQUIRE(doc.addBlock("gain", "G", false, name, err));
    CHECK(calls == 1);
    CHECK(lastRev == doc.revision());
    CHECK(lastOrigin == ChangeOrigin::Script);

    // A GUI commit reports the Gui origin (so the host can skip echoing it back).
    NFlowModelDocument builder("m");
    REQUIRE(NFlowModelDocument::fromJsonString(doc.toJsonString(), "m", builder, err));
    REQUIRE(builder.addBlock("sine", "S", false, name, err));
    REQUIRE(doc.replaceDiagram(builder.diagram(), ChangeOrigin::Gui, err));
    CHECK(calls == 2);
    CHECK(lastOrigin == ChangeOrigin::Gui);

    // Clearing the listener stops notifications.
    doc.setChangeListener(nullptr);
    REQUIRE(doc.setBlockParam("G", "Gain", "9", err));
    CHECK(calls == 2);
}
//=============================================================================
TEST_CASE("document typed parameters and geometry")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;
    REQUIRE(doc.addBlock("gain", "G", false, name, err));

    // A vector parameter round-trips as a JSON array.
    nlohmann::json vec = nlohmann::json::array({ 1.0, 2.0, 3.0 });
    REQUIRE(doc.setBlockParamJson("G", "Table", vec, err));
    nlohmann::json out;
    REQUIRE(doc.getBlockParamJson("G", "Table", out, err));
    CHECK(out.is_array());
    CHECK(out.size() == 3);
    CHECK(out[2].get<double>() == 3.0);

    // A matrix parameter (array of arrays) round-trips too.
    nlohmann::json mat = nlohmann::json::array(
        { nlohmann::json::array({ 1.0, 2.0 }), nlohmann::json::array({ 3.0, 4.0 }) });
    REQUIRE(doc.setBlockParamJson("G", "A", mat, err));
    REQUIRE(doc.getBlockParamJson("G", "A", out, err));
    CHECK(out.is_array());
    CHECK(out.size() == 2);
    CHECK(out[1][0].get<double>() == 3.0);

    // Read-only keys are rejected.
    CHECK_FALSE(doc.setBlockParamJson("G", "BlockType", vec, err));

    // Geometry: setBlockPosition stores [x,y] + size; getBlockPosition reports
    // [x y x2 y2].
    REQUIRE(doc.setBlockPosition("G", 10.0, 20.0, 60.0, 40.0, err));
    double x = 0, y = 0, x2 = 0, y2 = 0;
    REQUIRE(doc.getBlockPosition("G", x, y, x2, y2, err));
    CHECK(x == 10.0);
    CHECK(y == 20.0);
    CHECK(x2 == 70.0);
    CHECK(y2 == 60.0);
}
//=============================================================================
TEST_CASE("hierarchical editing by paths")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;

    REQUIRE(doc.addBlock("subsystem", "Sub", false, name, err));
    CHECK(name == "Sub");

    // A nested block is addressed by its "Sub/leaf" path.
    REQUIRE(doc.addBlock("gain", "Sub/G", false, name, err));
    CHECK(name == "Sub/G");
    CHECK(doc.hasBlock("Sub/G"));
    CHECK_FALSE(doc.hasBlock("G")); // not at the root

    REQUIRE(doc.setBlockParam("Sub/G", "gain", "5", err));
    std::string v;
    REQUIRE(doc.getBlockParam("Sub/G", "gain", v, err));
    CHECK(v == "5");
    REQUIRE(doc.setBlockPosition("Sub/G", 1.0, 2.0, 10.0, 20.0, err));
    double x = 0, y = 0, x2 = 0, y2 = 0;
    REQUIRE(doc.getBlockPosition("Sub/G", x, y, x2, y2, err));
    CHECK(x == 1.0);
    CHECK(x2 == 11.0);

    // A line inside the subsystem is stored with leaf names in Sub's diagram.
    REQUIRE(doc.addBlock("sine", "Sub/S", false, name, err));
    REQUIRE(doc.addLine("Sub/S", 1, "Sub/G", 1, true, err));

    // A cross-container line is rejected.
    REQUIRE(doc.addBlock("gain", "Top", false, name, err));
    CHECK_FALSE(doc.addLine("Sub/S", 1, "Top", 1, true, err));

    // A nested delete removes only the nested block, not a root sibling.
    REQUIRE(doc.deleteBlock("Sub/G", err));
    CHECK_FALSE(doc.hasBlock("Sub/G"));
    CHECK(doc.hasBlock("Sub/S"));
    CHECK(doc.hasBlock("Top"));

    // The flat model round-trips (nested graph is under params.subsystem).
    NFlowModelDocument copy("m");
    REQUIRE(NFlowModelDocument::fromJsonString(doc.toJsonString(), "m", copy, err));
    CHECK(copy.hasBlock("Sub/S"));
}
//=============================================================================
TEST_CASE("document layoutSystem arranges blocks into layers")
{
    loadCatalog();
    NFlowModelDocument doc("m");
    std::string name, err;
    REQUIRE(doc.addBlock("sine", "S", false, name, err));
    REQUIRE(doc.addBlock("gain", "G", false, name, err));
    REQUIRE(doc.addBlock("scope", "Sc", false, name, err));
    REQUIRE(doc.addLine("S", 1, "G", 1, true, err));
    REQUIRE(doc.addLine("G", 1, "Sc", 1, true, err));

    REQUIRE(doc.layoutSystem("", err));
    double xs = 0, ys = 0, xs2 = 0, ys2 = 0;
    double xg = 0, yg = 0, xg2 = 0, yg2 = 0;
    double xc = 0, yc = 0, xc2 = 0, yc2 = 0;
    REQUIRE(doc.getBlockPosition("S", xs, ys, xs2, ys2, err));
    REQUIRE(doc.getBlockPosition("G", xg, yg, xg2, yg2, err));
    REQUIRE(doc.getBlockPosition("Sc", xc, yc, xc2, yc2, err));
    // Strictly increasing columns along the chain.
    CHECK(xs < xg);
    CHECK(xg < xc);

    CHECK_FALSE(doc.layoutSystem("Nope", err));
}
//=============================================================================
TEST_CASE("registry: auto names, handles, dirty and close")
{
    auto& reg = NFlowModelRegistry::instance();
    reg.clear();
    double h1 = 0, h2 = 0;
    std::string n1, n2, err;
    REQUIRE(reg.createEmpty("", h1, n1, err));
    CHECK(n1 == "untitled");
    REQUIRE(reg.createEmpty("", h2, n2, err));
    CHECK(n2 == "untitled1");
    CHECK(h1 != h2);

    CHECK(reg.resolveByHandle(h1) != nullptr);
    CHECK(reg.resolveByName("untitled") != nullptr);
    CHECK(reg.isLoaded("untitled"));

    // Collision on explicit name.
    double h3 = 0;
    std::string n3;
    CHECK_FALSE(reg.createEmpty("untitled", h3, n3, err));

    REQUIRE(reg.close("untitled"));
    CHECK(reg.resolveByHandle(h1) == nullptr);
    CHECK_FALSE(reg.isLoaded("untitled"));
}
//=============================================================================
TEST_CASE("registry: current context tracks system and block")
{
    auto& reg = NFlowModelRegistry::instance();
    reg.clear();
    CHECK(reg.currentSystem().empty());
    CHECK(reg.currentBlock().empty());

    double h = 0;
    std::string n, err;
    REQUIRE(reg.createEmpty("ctx", h, n, err));
    reg.setCurrentSystem("ctx");
    reg.setCurrentBlock("ctx/Gain");
    CHECK(reg.currentSystem() == "ctx");
    CHECK(reg.currentBlock() == "ctx/Gain");

    // Closing the current model clears the context.
    REQUIRE(reg.close("ctx"));
    CHECK(reg.currentSystem().empty());
    CHECK(reg.currentBlock().empty());
}
//=============================================================================
TEST_CASE("registry: block handles allocate, resolve and invalidate (gcbh)")
{
    auto& reg = NFlowModelRegistry::instance();
    reg.clear();

    double mh = 0;
    std::string n, err;
    REQUIRE(reg.createEmpty("m", mh, n, err));

    // A block handle is allocated on demand and is idempotent per block path.
    double h1 = reg.handleForBlock("m/Gain");
    CHECK(reg.handleForBlock("m/Gain") == h1);

    // It draws from the same sequence as model handles, so it never collides.
    CHECK(h1 != mh);

    std::string path;
    CHECK(reg.resolveBlockHandle(h1, path));
    CHECK(path == "m/Gain");

    double h2 = reg.handleForBlock("m/Sum");
    CHECK(h2 != h1);

    // This is the path gcbh uses: current block -> its handle.
    reg.setCurrentBlock("m/Gain");
    CHECK(reg.handleForBlock(reg.currentBlock()) == h1);

    // Explicit invalidation (delete_block) drops just that handle.
    reg.invalidateBlockHandle("m/Gain");
    CHECK_FALSE(reg.resolveBlockHandle(h1, path));
    CHECK(reg.resolveBlockHandle(h2, path));

    // Closing the model invalidates the remaining block handles under it.
    REQUIRE(reg.close("m"));
    CHECK_FALSE(reg.resolveBlockHandle(h2, path));
}
//=============================================================================
