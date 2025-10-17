%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function build_help_from_path(module, module_path, lang, dirdest, package)
    if module == "main"
      main_help(module_path, lang, dirdest, package);
      return
    end
    src = [module_path, '/help/', lang, '/xml'];
    if ~isdir(src)
      src = [module_path, '/help/', getdefaultlanguage(), '/xml'];
    end
    if isdir(src)
      destination_path = [dirdest, '/', module];
      chapter_src_xml = [src, '/chapter.xml'];
      chapter_dest_xml = [destination_path, '/help_toc_summary.xml'];
      if ( package || ~isdir(destination_path) || need_to_be_generated(chapter_src_xml, chapter_dest_xml));
        mkdir(destination_path);
        [status, msg] = xmldoctohtml(src, destination_path, module, true);
        if ~status
          error(msg);
        end
        if package
            help_archive_name = [nelsonappid(), '.modules.', module, '.help.', lang, '.nhz'];
            destination_archive =  [module_path, '/help/', help_archive_name];
            zip(destination_archive, destination_path);
        end
        msg = sprintf(_('help "%s" (%s) generated.'), module, lang);
      else
        msg = sprintf(_('help "%s" (%s) is up to date.'), module, lang);
      end
      disp(msg);
    end
end
%=============================================================================
function main_help(module_path, lang, dirdest, package)
  destination_path = [dirdest, '/', 'main'];
  changes_md = {[nelsonroot(), '/CHANGELOG.md'];
  [nelsonroot(), '/CHANGELOG-0.7.x.md'];
  [nelsonroot(), '/CHANGELOG-0.6.x.md'];
  [nelsonroot(), '/CHANGELOG-0.5.x.md'];
  [nelsonroot(), '/CHANGELOG-0.4.x.md'];
  [nelsonroot(), '/CHANGELOG-0.3.x.md'];
  [nelsonroot(), '/CHANGELOG-0.2.x.md'];
  [nelsonroot(), '/CHANGELOG-0.1.x.md']};
  if ~isdir(destination_path)
    mkdir(destination_path);
  end
    dir_src = [nelsonroot(), '/modules/main/help/', lang, '/md'];
    if ~isdir(dir_src)
      src = [nelsonroot(), '/modules/main/help/', getdefaultlanguage(), '/xml'];
    end
    if isdir(dir_src)
      copyfile([modulepath('help_tools'), '/resources/', 'banner_homepage.png'], [destination_path , '/', 'banner_homepage.png']);
      copyfile([modulepath('help_tools'), '/resources/', 'banner_nelson_small.png'], [destination_path, '/banner_nelson_small.png']);
      copyfile([modulepath('help_tools'), '/resources/help_viewer.html' ], [destination_path, '/index.html']);
      copyfile([modulepath('help_tools'), '/resources/search_results.html' ], [destination_path, '/search_results.html']);
      copyfile([modulepath('help_tools'), '/resources/nelson_common.css' ], [destination_path, '/nelson_common.css']);
      copyfile([modulepath('help_tools'), '/resources/highlight.pack.js' ], [destination_path, '/highlight.pack.js']);
      copyfile([modulepath('help_tools'), '/resources/highlight.css' ], [destination_path, '/highlight.css']);
      copyfile([modulepath('help_tools'), '/resources/nelson_help.js'], [destination_path, '/nelson_help.js']);

      
      if ~isfile([dir_src, '/gpl-3.0.md'])
        copyfile([nelsonroot(), '/gpl-3.0.md'], [dir_src, '/gpl-3.0.md']);
      end
      if ~isfile([dir_src, '/lgpl-3.0.md'])
        copyfile([nelsonroot(), '/lgpl-3.0.md'], [dir_src, '/lgpl-3.0.md']);
      end
      md_files = {'getting_started.md', 'homepage.md', 'license.md', 'gpl-3.0.md', 'lgpl-3.0.md'};
      for md_filename = md_files(:)'
        html_filename = strrep(md_filename{1}, '.md', '.html');
        if need_to_be_generated([dir_src, '/', md_filename{1}], [destination_path, html_filename])
          if strcmp(md_filename{1}, 'homepage.md') || strcmp(md_filename{1}, 'license.md')
            content = fileread([dir_src, '/', md_filename{1}]);
            content = strrep(content, '(CHANGELOG.md)', '(CHANGELOG.html)');
            content = strrep(content, '(CHANGELOG-0.7.x.md)', '(CHANGELOG-0.7.x.html)');
            content = strrep(content, '(CHANGELOG-0.6.x.md)', '(CHANGELOG-0.6.x.html)');
            content = strrep(content, '(CHANGELOG-0.5.x.md)', '(CHANGELOG-0.5.x.html)');
            content = strrep(content, '(CHANGELOG-0.4.x.md)', '(CHANGELOG-0.4.x.html)');
            content = strrep(content, '(CHANGELOG-0.3.x.md)', '(CHANGELOG-0.3.x.html)');
            content = strrep(content, '(CHANGELOG-0.2.x.md)', '(CHANGELOG-0.2.x.html)');
            content = strrep(content, '(CHANGELOG-0.1.x.md)', '(CHANGELOG-0.1.x.html)');
            content = strrep(content, '(license.md)', '(license.html)');
            content = strrep(content, '(lgpl-3.0.md)', '(lgpl-3.0.html)');
            content = strrep(content, '(gpl-3.0.md)', '(gpl-3.0.html)');
            content = addStyle(content);
            content = addHRefScript(content);
            content_as_html = markdown(content, 'advanced');
            filewrite([destination_path, '/', html_filename], content_as_html);
          else
            try
              content = fileread([dir_src, '/', md_filename{1}]);
              content = addStyle(content);
              content = addHRefScript(content);
              content_as_html = markdown(content, 'advanced');
              filewrite([destination_path, '/', html_filename], content_as_html);
            catch
              error([_('file not generated:'), newline, html_filename]);
            end
          end
        end
      end
      for md = changes_md(:)'
        basename_src = fileparts(md{1}, 'filename');
        destination = [destination_path, '/', basename_src, '.html'];
        if need_to_be_generated(md{1}, destination)
          txt = fileread(md{1});
          txt = strrep(txt, '(CHANGELOG.md)', '(CHANGELOG.html)');
          txt = strrep(txt, '(CHANGELOG-0.7.x.md)', '(CHANGELOG-0.7.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.6.x.md)', '(CHANGELOG-0.6.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.5.x.md)', '(CHANGELOG-0.5.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.4.x.md)', '(CHANGELOG-0.4.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.3.x.md)', '(CHANGELOG-0.3.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.2.x.md)', '(CHANGELOG-0.2.x.html)');
          txt = strrep(txt, '(CHANGELOG-0.1.x.md)', '(CHANGELOG-0.1.x.html)');
          try
            txt = addStyle(txt);
            txt = addHRefScript(txt);
            html = markdown(txt, 'advanced');
            filewrite(destination, html);
          catch
          end
          if ~isfile(destination)
            error([_('file not generated:'), newline, destination]);
          end
        end
      end
      disp(['help "', 'homepage', '" (', lang, ') generated.']);
    end
    if package
      help_archive_name = [nelsonappid(), '.modules.main.help', '.', lang, '.nhz'];
      destination_archive =  [module_path, '/help/', help_archive_name];
      zip(destination_archive, destination_path);
    end
end
%=============================================================================
function content = addHRefScript(content)
script = [newline, '<script type="text/javascript">', newline, ...
'  document.addEventListener(''DOMContentLoaded'', function() {', newline, ...
'    document.querySelectorAll(''a[href^="http://"], a[href^="https://"]'').forEach(function(link) {', newline, ...
'      link.addEventListener(''click'', function(e) {', newline, ...
'        e.preventDefault();', newline, ...
'        var win = window.open(this.href, ''_blank'', ''noopener'');', newline, ...
'        if (win) win.focus();', newline, ...
'      });', newline, ...
'    });', newline, ...
'  });', newline, ...
'</script>', newline];
content = [content, script];
end
%=============================================================================
function content = addStyle(content)
  script = [newline, '<style>', newline, ...
  ':root {', newline, ...
    'color-scheme: light dark;', newline, ...
  '}', newline, ...
  'body {', newline, ...
    'background: #fff;', newline, ...
    'color: #222;', newline, ...
    'font-family: ''Segoe UI'', Arial, sans-serif;', newline, ...
    'line-height: 1.7;', newline, ...
    'margin: 0;', newline, ...
    'padding: 0 24px 40px 24px;', newline, ...
    'border-left: 4px solid rgba(0,0,0,0);', newline, ...
  '}', newline, ...
  'a {', newline, ...
    'color: #1976d2;', newline, ...
  '}', newline, ...
  'a:hover {', newline, ...
    'color: #0d47a1;', newline, ...
  '}', newline, ...
  'hr {', newline, ...
    'border-color: #e5e5e5;', newline, ...
  '}', newline, ...
  '@media (prefers-color-scheme: dark) {', newline, ...
    'body {', newline, ...
      'background: #181a1b;', newline, ...
      'color: #e0e0e0;', newline, ...
      'padding: 0 24px 40px 24px;', newline, ...
      'border-left: 4px solid rgba(255,255,255,0);', newline, ...
    '}', newline, ...
    'a {', newline, ...
      'color: #90caf9;', newline, ...
    '}', newline, ...
    'a:hover {', newline, ...
      'color: #42a5f5;', newline, ...
    '}', newline, ...
    'hr {', newline, ...
      'border-color: #333;', newline, ...
    '}', newline, ...
  '}', newline, ...
'</style>', newline];
  content = [script, content];
end