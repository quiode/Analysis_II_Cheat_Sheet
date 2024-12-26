
// global configuration
#set page(
  flipped: true,
  numbering: "1/1",
  columns: 3,
  paper: "a4",
  margin: 1cm,
  footer: context [
    #grid(
      columns: (1fr, 1fr, 1fr),
      align: (left, center, right),
      [Source: #link("https://gitlab.dominik-schwaiger.ch/quio/Analysis_II_Cheat_Sheet", "gitlab.dominik-schwaiger.ch")],
      [#counter(page).display(
          "1 of 1",
          both: true,
        )],
      [
        Version: #datetime.today().display("[day].[month].[year]")
      ],
    )
  ],
)
#set document(
  author: "Dominik Schwaiger",
  keywords: ("Spick", "Analysis", "Analysis II", "ETH", "Prüfung", "Exam", "Cheat Sheet"),
  title: "Analysis II Cheat Sheet",
)

= Analysis II Cheat Sheet

#lorem(2000)
