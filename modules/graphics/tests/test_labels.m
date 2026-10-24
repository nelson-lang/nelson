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
f = figure();
a = axes;
t1 = title('Global Title');
s = xlabel('X label')
ff = ylabel('Y label')
a.Visible = 'off';
t1.Visible = 'on';
s.Visible = 'on';
ff.Visible = 'on';
%=============================================================================
% an empty value clears a text String (xlabel([]) / set(h,'String',[]))
xlabel('something');
xlabel([]);
assert_isequal(char(get(gca(), 'XLabel').String), '');
ylabel([]);
assert_isequal(char(get(gca(), 'YLabel').String), '');
title('a title');
title([]);
assert_isequal(char(get(gca(), 'Title').String), '');
tx = text(0.5, 0.5, 'hi');
set(tx, 'String', []);
assert_istrue(isempty(char(tx.String)));
%=============================================================================
close all
%=============================================================================
