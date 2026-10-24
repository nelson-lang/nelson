//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export const LAYER_COUNT = 1;
export type ConnectionKey = string | number;
export interface RoutingSettings {
  maxTimeMs?: number;
  lengthCost?: number;
  turnCost?: number;
  hopCost?: number;
  nearObstacleDistance?: number;
  nearObstaclePenalty?: number;
  nearWirePenalty1?: number;
  nearWirePenalty2?: number;
  nearObstaclePenalty1?: number;
  nearObstaclePenalty2?: number;
  junctionPenalty?: number;
  nearPortTurnPenalty?: number;
  preferredPortStubCells?: number;
  incremental?: boolean;
  fullOptimize?: boolean;
  searchPadding?: number;
  changedConnections?: Iterable<ConnectionKey> | Array<ConnectionKey>;
  changedNodes?: Iterable<ConnectionKey> | Array<ConnectionKey>;
  /**
   * When true, wall-clock time caps are replaced by `maxExpansions` so results
   * are machine-independent (used by parity/golden tests).
   */
  deterministic?: boolean;
  /** Per-connection A* expansion cap used in deterministic mode. */
  maxExpansions?: number;
  /**
   * Route hysteresis: per-edge cost discount for reusing the connection's
   * previous route during the search (biases equal-cost choices toward the
   * old shape). 0 disables.
   */
  reuseBonus?: number;
  /**
   * Route hysteresis: keep the previous route unless the new one improves
   * its true cost by at least this fraction (e.g. 0.15 = 15%). 0 disables.
   */
  acceptThreshold?: number;
  /**
   * Rip-up-and-reroute refinement iterations after the main pass.
   * Unset → 2 for full routes, 0 for incremental ones.
   */
  refineIterations?: number;
}
//=============================================================================
export function RoutingConfig(settings: RoutingSettings = {}) {
  // Merges the settings with the defaults; changedConnections and changedNodes
  // come back as Sets.
  const defaults = {
    maxTimeMs: 200,
    lengthCost: 1,
    turnCost: 6,
    hopCost: 20,
    nearObstacleDistance: 0,
    nearObstaclePenalty: 0,
    nearWirePenalty1: 0,
    nearWirePenalty2: 0,
    nearObstaclePenalty1: 0,
    nearObstaclePenalty2: 0,
    junctionPenalty: 4,
    nearPortTurnPenalty: 40,
    preferredPortStubCells: 2,
    incremental: false,
    fullOptimize: true,
    searchPadding: 20,
    deterministic: false,
    maxExpansions: 400000,
    reuseBonus: 0.2,
    acceptThreshold: 0.15,
  } as const;

  const merged = Object.assign({}, defaults, settings) as unknown as RoutingSettings & {
    changedConnections: Set<ConnectionKey>;
    changedNodes: Set<ConnectionKey>;
  };

  const normalizeToSet = (v: unknown) => {
    if (!v) return new Set<ConnectionKey>();
    if (v instanceof Set) return v as Set<ConnectionKey>;
    if (Array.isArray(v)) return new Set(v as ConnectionKey[]);
    try {
      return new Set(Array.from(v as Iterable<ConnectionKey>));
    } catch {
      return new Set<ConnectionKey>();
    }
  };

  merged.changedConnections = normalizeToSet((settings as RoutingSettings).changedConnections);
  merged.changedNodes = normalizeToSet((settings as RoutingSettings).changedNodes);

  return merged;
}
//=============================================================================
