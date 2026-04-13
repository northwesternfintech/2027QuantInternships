open Types

let make_url url label =
  if String.length url = 0 then "not available" else Printf.sprintf "[%s](%s)" label url
;;

module T = struct
  let dump_company_name (company : Types.Company.t) =
    Printf.sprintf "## %s\n" company.name
  ;;

  let dump_company_website (company : Types.Company.t) =
    Printf.sprintf "**Website**: %s\n\n" (make_url company.website company.name)
  ;;

  let dump_company_locations (company : Types.Company.t) =
    Printf.sprintf "**Locations**: %s\n\n" company.locations
  ;;

  let dump_company_notes (company : Types.Company.t) =
    Printf.sprintf "**Notes**: %s\n\n" company.notes
  ;;

  let dump_links links =
    let separator = "&nbsp;&nbsp;&nbsp;&nbsp;" in
    links
    |> List.map (fun link ->
      let label =
        match link.label with
        | Some s -> s
        | None -> ""
      in
      make_url link.url ("✅ " ^ label))
    |> String.concat separator
  ;;

  let dump_single_role role =
    Printf.sprintf "|%s|%s|" role.role_type (dump_links role.links)
  ;;

  let dump_company_roles (company : Types.Company.t) =
    let header = "|Role|Links|" ^ "\n" ^ "|-------|-------|" ^ "\n" in
    let role_strings = company.roles |> List.map dump_single_role |> String.concat "\n" in
    header ^ role_strings
  ;;

  let dumps (company : Types.Company.t) =
    dump_company_name company
    ^ dump_company_website company
    ^ dump_company_locations company
    ^ dump_company_notes company
    ^ dump_company_roles company
    ^ "\n"
  ;;
end
