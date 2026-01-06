#import "framing.typ": *
#import "formula.typ": * // constrain later

// returns an array of each formula's array of frame lines; core logic of the library.
#let frame-as-array(lines, fl-model) = {

  let frm = ()
  let new = (fl-model,)
  let is-assume = (false,)

  for line in lines {

    if line == start {

      if is-assume.last() {
        let fl = fl-model
        fl.insert("is-short", true)
        new.push(fl)
        }
        
      else {new.push(fl-model)}
      is-assume.push(false)

    }

    else if line == end {
      
      let _ = new.pop()
      let _ = is-assume.pop()
      
    }

    else if line == assume {
      frm.last().last().insert("is-assume", true)
      is-assume.last() = true
    }

    // is a visible line
    else {
      frm.push(new)
      new.last().insert("is-short", false)
    }

  }

  return frm

}

// (internal use) returns an array of the ranges of the assumptions' indices in the proof
#let assumption-ranges(lines) = {

  let starts = if lines.first() == start {()} else {(0,)}
  let assumes = ()
  let equation-counter = 0

  for line in lines {

    if line == start {starts.push(equation-counter)}
    else if line == assume {assumes.push(equation-counter)}
    else if line not in utils {equation-counter += 1}

  }

  return array.zip(starts,assumes)
  

  // note: I think it only works if an assume comes after every start, which is syntactically required, but won't always be the case, such as while writing. Requires input verification or handling or the qed idea or a logic that makes it work in more cases. 

}

// constructs the chart
#let chart(fl-model, assumption-mode, lines) = context {



  let arr = frame-as-array(lines, fl-model)
  let formulas = lines.filter(x => x not in utils)
  
  if assumption-mode == "dynamic" {
    
  let assumption-ranges = assumption-ranges(lines)
  let assumption-chunks = assumption-ranges.map(it => 
  formulas.slice(it.first(), it.last()))

  let widest-per-range = assumption-chunks.map(it =>
    calc.max(..it.map(x => 
       measure(x.equation).width)
       )
     ).rev() // reversing allows to use .pop() concisely

    for (_, to) in assumption-ranges {
      arr.at(to - 1).last().insert("assume-length", widest-per-range.pop() + 1em)
    }

  }

  else if assumption-mode == "dynamic-single" {
    for i in range(arr.len()) {
      arr.at(i).last().insert("assume-length", measure(formulas.at(i).equation).width + 1em) // cursed, couldn't find another way
    }
  }
  
  else if assumption-mode == "widest" {
    let widest = calc.max(..formulas.map(it => measure(it.equation).width))
    for i in range(arr.len()) {
      arr.at(i).last().insert("assume-length", widest + 1em)
    }
  }

  let merged = array.zip(arr, formulas)
  let table = ()

  for (fl-row, formula) in merged {

    let partition = fl-row.map(it => fl-model.thick + .75em) // length partition of the row
    partition.last() = .375em // last framing in row
    partition.push(calc.max(
      measure(formula.equation).width,
      if fl-row.last().is-assume {measure(h(fl-row.last().assume-length)).width}
      else {-1pt}
      )) // cursed
    // length of equation + frame

    let new-line = grid(
      columns: partition,
      rows: fl-model.height,
      align: left+bottom,
      stroke: none,
      ..fl-row.map(it => fl-display(it)),
      grid.cell(align: left+horizon, formula.equation)

    )
    table.push(new-line)
  }

  let indices = formulas.map(it => it.index)
  let rules = formulas.map(it => it.rule)

  let widest-index = calc.max(..indices.map(it => measure(it).width))
  let widest-line = calc.max(..table.map(it => measure(it).width))
  let widest-rule = calc.max(..rules.map(it => measure(it).width))

  let rows = array.zip(indices, table, rules)

  return grid(
    columns: (widest-index, widest-line, widest-rule),
    rows: fl-model.height,
    align: (right+horizon, left+horizon, left+horizon),
    column-gutter: (.75em,2em),
    stroke: none,
    ..rows.flatten()
  )


}