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
%=============================================================================
plot(rand(5, 5))
f = gcf();
try
  copygraphics(f);
catch ex
  close(f);
  skip_testsuite(strcmp(ex.message, _('Cannot copy the figure to the clipboard.')), ...
                 _('Native clipboard unavailable in this GUI test host.'));
  rethrow(ex);
end
close(f);
%=============================================================================
