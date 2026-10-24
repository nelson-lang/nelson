%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlOverview(cases)
  lines = {
    '<section class="overview-grid" aria-label="Report overview">'
    '<div class="overview-panel">'
  };
  lines = [lines(:); report_htmlSlowest(cases)];
  lines = [lines; {'</div>'; '<div class="overview-panel">'}];
  lines = [lines; report_htmlModuleSummary(cases)];
  lines = [lines; {'</div>'; '</section>'}];
  lines = lines(:);
end
%=============================================================================
