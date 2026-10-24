%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
binpath = modulepath('nelson', 'bin');
executable = [binpath, '/nelson-adv-cli'];
tic();
[status, output] = run_nelson_cli(executable, ...
  ' --quiet --no-user-startup --no-ipc --timeout 2 -e "figure(); surf(peaks(8)); drawnow(); while true; end"');
elapsed = toc();
if ispc()
  asserts.isequal(status, 258);
else
  asserts.isequal(status, 134);
end
asserts.isfalse(contains(output, 'Segmentation fault'));
asserts.istrue(elapsed >= 2 && elapsed < 10);
