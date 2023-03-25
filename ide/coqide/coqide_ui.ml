let ui_m = GAction.ui_manager ();;

let no_under = Util.String.map (fun x -> if x = '_' then '-' else x)

let list_items menu li =
  let res_buf = Buffer.create 500 in
  let tactic_item = function
    |[] -> Buffer.create 1
    |[s] -> let b = Buffer.create 16 in
            let () = Buffer.add_string b ("<menuitem action='"^menu^" "^(no_under s)^"' />\n") in
            b
    |s::_ as l -> let b = Buffer.create 50 in
                  let () = (Buffer.add_string b ("<menu action='"^menu^" "^(String.make 1 s.[0])^"'>\n")) in
                  let () = (List.iter
                             (fun x -> Buffer.add_string b ("<menuitem action='"^menu^" "^(no_under x)^"' />\n")) l) in
                  let () = Buffer.add_string b"</menu>\n" in
                  b in
  let () = List.iter (fun b -> Buffer.add_buffer res_buf (tactic_item b)) li in
  res_buf

let list_queries menu li =
  let res_buf = Buffer.create 500 in
  let query_item (q, _) =
    let s = "<menuitem action='"^menu^" "^(no_under q)^"' />\n" in
    Buffer.add_string res_buf s
  in
  let () = List.iter query_item li in
  res_buf

let init () =
  let theui = Printf.sprintf {|<ui>
<menubar name='CoqIDE MenuBar'>
  <menu action='File'>
    <menuitem action='New' />
    <menuitem action='Open' />
    <menuitem action='Save' />
    <menuitem action='Save as' />
    <menuitem action='Save all' />
    <menuitem action='Close buffer' />
    <menuitem action='Print...' />
    <menu action='Export to'>
      <menuitem action='Html' />
      <menuitem action='Latex' />
      <menuitem action='Dvi' />
      <menuitem action='Pdf' />
      <menuitem action='Ps' />
    </menu>
    <menuitem action='Rehighlight' />
    %s
  </menu>
  <menu name='Edit' action='Edit'>
    <menuitem action='Undo' />
    <menuitem action='Redo' />
    <separator />
    <menuitem action='Cut' />
    <menuitem action='Copy' />
    <menuitem action='Paste' />
    <separator />
    <menuitem action='Find' />
    <menuitem action='Find Next' />
    <menuitem action='Find Previous' />
    <separator />
    <menuitem action='External editor' />
    <separator />
    <menuitem name='Prefs' action='Preferences' />
  </menu>
  <menu name='View' action='View'>
    <menuitem action='Previous tab' />
    <menuitem action='Next tab' />
    <separator/>
    <menuitem action='Zoom in' />
    <menuitem action='Zoom out' />
    <menuitem action='Zoom fit' />
    <separator/>
    <menuitem action='Show Toolbar' />
    <menuitem action='Query Pane' />
    <separator/>
    <menuitem action='Display implicit arguments' />
    <menuitem action='Display coercions' />
    <menuitem action='Display nested matching expressions' />
    <menuitem action='Display notations' />
    <menuitem action='Display parentheses' />
    <menuitem action='Display all basic low-level contents' />
    <menuitem action='Display existential variable instances' />
    <menuitem action='Display universe levels' />
    <menuitem action='Display all low-level contents' />
    <menuitem action='Display unfocused goals' />
    <menuitem action='Display goal names' />
    <menuitem action='Use record syntax' />
    <menuitem action='Hide synthesizable types' />
    <separator/>
    <menuitem action='Unset diff' />
    <menuitem action='Set diff' />
    <menuitem action='Set removed diff' />
    <menuitem action='Show Proof Diffs' />
  </menu>
  <menu action='Navigation'>
    <menuitem action='Forward' />
    <menuitem action='Backward' />
    <menuitem action='Run to cursor' />
    <menuitem action='Run to end' />
    <menuitem action='Interrupt' />
    <menuitem action='Reset' />
    <menuitem action='Previous' />
    <menuitem action='Next' />
    <menuitem action='Force' />
  </menu>
  <menu action='Templates'>
    <menuitem action='Lemma' />
    <menuitem action='Theorem' />
    <menuitem action='Definition' />
    <menuitem action='Inductive' />
    <menuitem action='Fixpoint' />
    <menuitem action='Scheme' />
    <menuitem action='match' />
    <separator />
    %s
  </menu>
  <menu action='Queries'>
    <menuitem action='Search' />
    <menuitem action='Check' />
    <menuitem action='Print' />
    <menuitem action='About' />
    <menuitem action='Locate' />
    <menuitem action='Print Assumptions' />
    <separator />
    %s
  </menu>
  <menu name='Tools' action='Tools'>
    <menuitem action='Comment' />
    <menuitem action='Uncomment' />
    <separator />
    <menuitem action='Coqtop arguments' />
    <separator />
    <menuitem action='LaTeX-to-unicode' />
  </menu>
  <menu action='Compile'>
    <menuitem action='Compile buffer' />
    <menuitem action='Make' />
    <menuitem action='Next error' />
    <menuitem action='Make makefile' />
  </menu>
  <menu action='Debug'>
    <menuitem action='Toggle breakpoint' />
    <menuitem action='Continue' />
    <menuitem action='Step in' />
    <menuitem action='Step out' />
    <menuitem action='Break' />
    <menuitem action='Show debug panel' />
  </menu>
  <menu action='Windows'>
    <menuitem action='Detach Proof' />
  </menu>
  <menu name='Help' action='Help'>
    <menuitem action='Browse Coq Manual' />
    <menuitem action='Browse Coq Library' />
    <menuitem action='Help for keyword' />
    <menuitem action='Help for μPG mode' />
    <separator />
    <menuitem name='Abt' action='About Coq' />
  </menu>
</menubar>
<toolbar name='CoqIDE ToolBar'>
  <toolitem action='New' />
  <toolitem action='Open' />
  <toolitem action='Save' />
  <toolitem action='Close buffer' />
  <separator />
  <toolitem action='Forward' />
  <toolitem action='Backward' />
  <toolitem action='Run to cursor' />
  <toolitem action='Run to end' />
  <toolitem action='Force' />
  <toolitem action='Interrupt' />
  <toolitem action='Reset' />
  <toolitem action='Previous' />
  <toolitem action='Next' />
</toolbar>
</ui>|}
    (if Config.gtk_platform <> `QUARTZ then "<menuitem action='Quit' />" else "")
    (Buffer.contents (list_items "Template" Coq_commands.commands))
    (Buffer.contents (list_queries "User-Query" Preferences.user_queries#get))
 in
  ignore (ui_m#add_ui_from_string theui);
