%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('ngen'), nmm('load', 'ngen'); end

% ngen (SLICOT gateway generation) - run: install ngen via nmm first


FILES = {
  'nig_sb10jd.json';
  'nig_mb02md.json';
  'nig_sg02ad.json';
  'nig_sb04qd.json';
  'nig_sb04md.json';
  'nig_mb03od.json';
  'nig_mb03rd.json';
  'nig_mb03pd.json';
  'nig_ab01od.json';
  'nig_ab04md.json';
  'nig_ab08nd.json';
  'nig_sb01bd.json';
  'nig_sb03od.json';
  'nig_sb03md.json';
  'nig_sb02od.json';
  'nig_mb04md.json';
  'nig_ab07nd.json';
  'nig_ab07nd.json';
  'nig_ag08bd.json';
  'nig_tg01ad.json';
  'nig_mc01td.json';
  'nig_mb04gd.json';
  'nig_mb05od.json';
  'nig_tb01id.json';
};

NGEN_FUNCTIONS = struct([]);
for f = FILES(:)'
  NGEN_FUNCTION = jsondecode(f{1}, '-file');
  if isempty(NGEN_FUNCTIONS)
    NGEN_FUNCTIONS = NGEN_FUNCTION;
  else
    NGEN_FUNCTIONS = [NGEN_FUNCTIONS; NGEN_FUNCTION];
  end
end

ngen.gateway(NGEN_FUNCTIONS, modulepath('slicot'))
