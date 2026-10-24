%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% report = NFlow.internal.acausalParityReport()
%
% Acausal palette N8: the Coselica-parity checklist, computed from the
% single-source catalog. Cross-references the implemented components
% (NFlow.internal.acausalCatalog) against the Coselica palette target per domain and
% reports coverage. Prints a readable table and returns a struct with the
% per-domain counts, so the parity status is a queryable fact, not a stale
% hand-maintained list.
%
% The `target` counts are the sizes of the Coselica palette groups (extracted
% from Coselica.mo, used as a factual catalog only). Coverage counts curated,
% engine-backed components; the remaining are the smooth-only / event-tier /
% planar items.
%=============================================================================
function report = acausalParityReport()
  catalog = NFlow.internal.acausalCatalog();
  % Coselica palette group sizes (target), per physical domain.
  domains = {'electrical', 'thermal', 'translational', 'rotational', 'planar'};
  targets = [54, 16, 41, 50, 46];

  report = struct();
  report.domains = {};
  disp('nflow acausal palette - Coselica parity (from NFlow.internal.acausalCatalog):');
  disp('  domain          implemented   target   note');
  totalImpl = 0; totalTarget = 0;
  for i = 1:numel(domains)
    d = domains{i};
    n = 0; names = {};
    for k = 1:numel(catalog)
      if strcmp(catalog(k).domain, d)
        n = n + 1; names{end + 1} = catalog(k).name; %#ok<AGROW>
      end
    end
    note = '';
    if strcmp(d, 'planar')
      % Planar blocks live in the native planarMechanicalIsland engine and the
      % frame-diagram expansion (NFlow.internal.planarExpand), sourced from the planar
      % catalog (not the scalar-node acausal catalog), plus CutForce joint
      % reaction outputs. Closed kinematic loops (e.g. four-bar linkages) need
      % no special loop joint; they are consistent Revolute constraints.
      pcat = NFlow.internal.planarCatalog();
      names = {pcat.name};
      names{end + 1} = 'CutForce';
      n = numel(names);
      note = 'N7 engine + frame expansion + closed loops (core joints/sensors/forces)';
    elseif n < targets(i)
      note = 'tier-A/B done; events + variants remain';
    else
      note = 'core covered';
    end
    disp(sprintf('  %-14s  %6d       %6d   %s', d, n, targets(i), note));
    e = struct('domain', d, 'implemented', n, 'target', targets(i), 'names', {names});
    report.domains{end + 1} = e; %#ok<AGROW>
    totalImpl = totalImpl + n;
    totalTarget = totalTarget + targets(i);
  end
  % Causal signal blocks (Coselica Blocks.*): tracked separately (they are
  % ordinary nflow blocks, not acausal islands).
  report.signalParity = 'complete (mathFunction, pulse/sawTooth/expSine/trapezoid/exponentials, hysteresis, vector sum/dot/cross; LimPID via pid)';
  report.totalImplemented = totalImpl;
  report.totalTarget = totalTarget;
  disp(sprintf('  %-14s  %6d       %6d', 'TOTAL (phys)', totalImpl, totalTarget));
  disp(['  signal blocks : ', report.signalParity]);
end
%=============================================================================
