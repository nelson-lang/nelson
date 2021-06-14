%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function buildhelp(varargin)
  % build Nelson help files
  % buildhelp() build help of Nelson
  % buildhelp(module_name) build help of a module loaded in Nelson
  %
  if nargin() > 1
    error(_('Wrong number of input arguments.'));
  end
  if ismodule('help_browser')
    helpbrowser('-clearcache');
  end
  pref_help = [prefdir(), '/help_index.nh5'];
  if isfile(pref_help)
    rmfile(pref_help);
  end
  if nargin() == 0
    helpForNelsonOnly()
    run([nelsonroot() '/modules/' 'modules.nls']);

    for m = modules_help_list(:)'
      module_path = [nelsonroot() '/modules/' m{1}];
      buildhelp_from_path(m{1}, module_path);
    end
  else
    module = varargin{1};
    module_path = modulepath(module);
    buildhelp_from_path(module, module_path);
  end
end
%=============================================================================
function buildhelp_from_path(module, module_path)
  for k = getavailablelanguages()(:)'
    src = [module_path, '/help/', k{1}, '/xml'];
    if isdir(src)
      if ispc() || ismac()
        dstbuild = [tempdir(), 'build_help/modules/', module, '/help/', k{1}, '/'];
      else
        username = getenv('USER');
        dstbuild = [tempdir(), '', username, '/build_help/modules/', module, '/help/', k{1}, '/'];
      end
      if isdir(dstbuild)
        [res, msg] = rmdir(dstbuild, 's');
        if ~res
          error(msg);
        end
      end
      res = mkdir(dstbuild);
      if ~res
        error(msg);
      end
      p = xmldoctohelp(src, dstbuild, module);
      if isfile(p)
        dst = [module_path, '/help/', k{1}];
        if copyfile(p, dst, 'f')
          disp(['help ''', module, ''' (', k{1}, ') generated.']);
        else
          disp(_('Impossible to copy help file generated.'));
        end
      else
        disp([module, _(' file was not generated.')]);
      end
    end
  end
end
%=============================================================================
function helpForNelsonOnly()
  changes_md = {[nelsonroot(), '/CHANGELOG.md'];
                [nelsonroot(), '/CHANGELOG-0.4.x.md'];
                [nelsonroot(), '/CHANGELOG-0.3.x.md'];
                [nelsonroot(), '/CHANGELOG-0.2.x.md'];
                [nelsonroot(), '/CHANGELOG-0.1.x.md']};
  for k = getavailablelanguages()(:)'
    dir_src = [nelsonroot(), '/modules/main/help/', k{1}, '/md'];
    dir_dst = [nelsonroot(), '/modules/main/help/', k{1}, '/html/'];
    if isdir(dir_src)
      banner_png = 'banner_homepage.png';
      copyfile([dir_src, '/', banner_png], [dir_dst, '/', banner_png]);
      md_files = {'homepage.md', 'license.md', 'GPL2.md', 'LGPL21.md'};
      for md_filename = md_files(:)'
        html_filename = strrep(md_filename{1}, '.md', '.html');
        if needTobeGenerated([dir_src, '/', md_filename{1}], [dir_dst, html_filename])
          if strcmp(md_filename{1}, 'homepage.md') || strcmp(md_filename{1}, 'license.md')
            content = fileread([dir_src, '/', md_filename{1}]);
            content = strrep(content, '(CHANGELOG.md)', '(CHANGELOG.html)');
            content = strrep(content, '(CHANGELOG-0.4.x.md)', '(CHANGELOG-0.4.x.html)');
            content = strrep(content, '(CHANGELOG-0.3.x.md)', '(CHANGELOG-0.3.x.html)');
            content = strrep(content, '(CHANGELOG-0.2.x.md)', '(CHANGELOG-0.2.x.html)');
            content = strrep(content, '(CHANGELOG-0.1.x.md)', '(CHANGELOG-0.1.x.html)');
            content = strrep(content, '(license.md)', '(license.html)');
            content = strrep(content, '(LGPL21.md)', '(LGPL21.html)');
            content = strrep(content, '(GPL2.md)', '(GPL2.html)');
            content_as_html = markdown(content);
            filewrite([dir_dst, html_filename], content_as_html);
          else
            r = markdown([dir_src, '/', md_filename{1}], [dir_dst, '/', html_filename]);
            if (r == false)
              error([_('file not generated:'), newline, html_filename]);
            end
          end
        end
      end
      for md = changes_md(:)'
        basename_src = fileparts(md{1}, 'filename');
        destination = [dir_dst, basename_src, '.html'];
        if needTobeGenerated(md{1}, destination)
          txt = fileread(md{1});
          txt = strrep(txt, '(CHANGELOG.md)', '(CHANGELOG.html)');
          txt = strrep(txt, '(CHANGELOG-0.3.x.md)', '(CHANGELOG-0.3.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.2.x.md)', '(CHANGELOG-0.2.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.1.x.md)', '(CHANGELOG-0.1.x.html)');
          try
            html = markdown(txt);
            filewrite(destination, html);
          catch
          end
          if ~isfile(destination)
            error([_('file not generated:'), newline, destination]);
          end
        end
      end
      qhelpgenerator(dir_dst, [nelsonroot(), '/modules/main/help/', k{1} , '/'], 'org.nelson.help.qch')
      disp(['help ''', 'homepage', ''' (', k{1}, ') generated.']);
    end
  end
end
%=============================================================================
function tf = needTobeGenerated(md_filename, html_filename)
  if isfile(html_filename)
    html_file_info = dir(html_filename);
    md_file_info = dir(md_filename);
    tf = md_file_info.datenum > html_file_info.datenum;
  else
    tf = true;
  end
end
%=============================================================================
