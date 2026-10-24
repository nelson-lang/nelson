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
% Setting Parent on a UI component or a uicontrol follows the constructor rule:
% a rejected parent raises the constructor error and leaves the object where
% it was.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
g = hggroup('Parent', ax);
invalidParentMsg = getString(message('Nelson:uicomponent:invalidParent'));
%=============================================================================
b = uibutton(f);
assert_checkerror('b.Parent = ax;', invalidParentMsg, 'Nelson:uicomponent:invalidParent');
assert_checkerror('set(b, ''Parent'', g);', invalidParentMsg, 'Nelson:uicomponent:invalidParent');
assert_istrue(b.Parent == f);
assert_istrue(any(f.Children == b));
assert_isfalse(any(ax.Children == b));
assert_isfalse(any(g.Children == b));
%=============================================================================
p = uipanel('Parent', f);
assert_checkerror('p.Parent = ax;', invalidParentMsg, 'Nelson:uicomponent:invalidParent');
assert_istrue(p.Parent == f);
assert_isfalse(any(ax.Children == p));
%=============================================================================
l = uilabel(f);
assert_checkerror('l.Parent = g;', invalidParentMsg, 'Nelson:uicomponent:invalidParent');
assert_istrue(l.Parent == f);
%=============================================================================
c = uicontrol('Parent', f);
% same error as the constructor
try
  uicontrol('Parent', ax);
  constructorError = [];
catch constructorError
end
assert_isfalse(isempty(constructorError));
assert_checkerror('c.Parent = g;', constructorError.message, constructorError.identifier);
assert_checkerror('c.Parent = ax;', constructorError.message, constructorError.identifier);
assert_istrue(c.Parent == f);
assert_istrue(any(f.Children == c));
assert_isfalse(any(g.Children == c));
%=============================================================================
% a container cannot become its own descendant
inner = uipanel(p);
assert_checkerror('p.Parent = inner;', invalidParentMsg, 'Nelson:uicomponent:invalidParent');
assert_istrue(p.Parent == f);
%=============================================================================
% uitab keeps its own rule: only a uitabgroup holds it
tg = uitabgroup(f);
t = uitab(tg);
assert_checkerror('t.Parent = f;', getString(message('Nelson:uicomponent:parentMustBeTabGroupObject')), 'Nelson:uicomponent:parentMustBeTabGroupObject');
assert_checkerror('t.Parent = p;', getString(message('Nelson:uicomponent:parentMustBeTabGroupObject')), 'Nelson:uicomponent:parentMustBeTabGroupObject');
assert_istrue(t.Parent == tg);
tg2 = uitabgroup(f);
t.Parent = tg2;
assert_istrue(t.Parent == tg2);
assert_isfalse(any(tg.Children == t));
% and a uitabgroup holds nothing else
assert_checkerror('b.Parent = tg;', getString(message('Nelson:uicomponent:parentMustBeTabGroup')), 'Nelson:uicomponent:parentMustBeTabGroup');
assert_istrue(b.Parent == f);
%=============================================================================
% radio and toggle buttons stay in a button group
bg = uibuttongroup(f);
r = uiradiobutton(bg);
assert_checkerror('r.Parent = p;', getString(message('Nelson:uicomponent:parentMustBeButtonGroup')), 'Nelson:uicomponent:parentMustBeButtonGroup');
assert_istrue(r.Parent == bg);
%=============================================================================
% valid re-parenting: figure -> uipanel -> figure
b.Parent = p;
assert_istrue(b.Parent == p);
assert_istrue(any(p.Children == b));
assert_isfalse(any(f.Children == b));
b.Parent = f;
assert_istrue(b.Parent == f);
assert_istrue(any(f.Children == b));
assert_isfalse(any(p.Children == b));
%=============================================================================
close(f);
%=============================================================================
