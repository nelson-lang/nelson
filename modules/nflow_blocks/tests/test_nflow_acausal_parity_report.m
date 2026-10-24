%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Acausal palette N8: NFlow.internal.acausalParityReport computes the Coselica-parity
% coverage from the single-source catalog and returns a queryable struct.
%=============================================================================
report = NFlow.internal.acausalParityReport();
assert_istrue(isstruct(report));
assert_istrue(numel(report.domains) == 5); % electrical/thermal/transl/rot/planar
% Each domain entry has implemented <= target. The scalar-node domains count
% their entries from the acausal catalog; the planar domain is counted from the
% planar catalog (+ CutForce) because its blocks use the frame model.
catalog = NFlow.internal.acausalCatalog();
for i = 1:numel(report.domains)
  e = report.domains{i};
  assert_istrue(e.implemented <= e.target);
  if strcmp(e.domain, 'planar')
    assert_istrue(e.implemented == numel(NFlow.internal.planarCatalog()) + 1);
  else
    n = 0;
    for k = 1:numel(catalog)
      if strcmp(catalog(k).domain, e.domain); n = n + 1; end
    end
    assert_istrue(e.implemented == n);
  end
end
% Electrical and planar both have coverage (> 0).
elec = report_domain(report, 'electrical');
assert_istrue(elec.implemented > 0);
planar = report_domain(report, 'planar');
assert_istrue(planar.implemented > 0);
assert_istrue(report.totalImplemented > 0);
assert_istrue(~isempty(report.signalParity));
%=============================================================================
function e = report_domain(report, d)
  e = [];
  for i = 1:numel(report.domains)
    if strcmp(report.domains{i}.domain, d); e = report.domains{i}; return; end
  end
end
%=============================================================================
