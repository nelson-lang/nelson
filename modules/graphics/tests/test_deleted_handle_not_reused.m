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
% A deleted graphics object stays invalid: objects created afterwards never
% take its handle, so a stale handle cannot reach them.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
t = text(ax, 0.5, 0.5, 'hi');
l = line(ax, 1:3, 1:3);
cla(ax);
b = bar(ax, 1:3);
p = line(ax, 1:3, 3:-1:1);
assert_isfalse(isvalid(t));
assert_isfalse(isvalid(l));
assert_istrue(isvalid(b));
assert_istrue(isvalid(p));
assert_istrue(getFails(t, 'Type'));
assert_istrue(getFails(l, 'Type'));
assert_istrue(setFails(t, 'String', 'x'));
assert_isequal(get(b, 'Type'), 'bar');
%=============================================================================
h = gobjects(1, 50);
for k = 1:50
  h(k) = line(ax, 1:2, 1:2);
end
delete(h(1:25));
n = line(ax, 1:2, 1:2);
assert_isfalse(any(arrayfun(@(x) isequal(x, n), h(1:25))));
assert_isfalse(any(isvalid(h(1:25))));
assert_istrue(all(isvalid(h(26:50))));
delete(f);
%=============================================================================
function failed = getFails(h, name)
  failed = false;
  try
    get(h, name);
  catch
    failed = true;
  end
end
%=============================================================================
function failed = setFails(h, name, value)
  failed = false;
  try
    set(h, name, value);
  catch
    failed = true;
  end
end
%=============================================================================
