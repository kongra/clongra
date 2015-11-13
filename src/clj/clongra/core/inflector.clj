;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2012-09-26

(in-ns 'clongra.core)

;; A PART OF clongra.core CONTAINING THE CONFIGURATION FOR THE
;; INFLECTOR LIBRARY.

;; PUT IRREGULAR NOUNS HERE

(def ^:private IRREGULAR-NOUNS
  '{
    ;; Some nouns that end in -f or -fe are changed to -ves in the
    ;; plural
    calf calves
    elf elves
    half halves
    hoof hooves
    knife knives
    leaf leaves
    life lives
    loaf loaves
    scarf scarves
    self selves
    sheaf sheaves
    shelf shelves
    thief thieves
    wife wives
    wolf wolves

    ;; Some nouns change the vowel sound in becoming plural
    fireman firemen
    foot feet
    goose geese
    louse lice
    man men
    mouse mice
    tooth teeth
    woman women

    ;; Some Old English plurals are still in use
    child children
    ox oxen

    ;; Some nouns ending in -o take -s as the plural, while others
    ;; take -es
    auto autos
    kangaroo kangaroos
    kilo kilos
    memo memos
    photo photos
    piano pianos
    pimento pimentos
    pro pros
    solo solos
    soprano sopranos
    studio studios
    tattoo tattoos
    video videos
    zoo zoos

    echo echoes
    embargo embargoes
    hero heroes
    potato potatoes
    tomato tomatoes
    torpedo torpedoes
    veto vetoes

    ;; Some nouns ending in -o take either -s or -es
    buffalo buffalos
    cargo cargos
    halo halos
    mosquito mosquitos
    motto mottos
    no nos
    tornado tornados
    volcano volcanos
    zero zeros

    ;; Some nouns do not change at all
    cod cod
    deer deer
    fish fish
    offspring offspring
    perch perch
    sheep sheep
    trout trout

    ;; These include nouns that are traditionally plural, but are also
    ;; used for singular forms
    barracks barracks
    crossroads crossroads
    dice dice
    gallows gallows
    headquarters headquarters
    means means
    series series
    species species

    ;; Other nouns retain foreign plurals. Note that some of these
    ;; have adapted a regular English plural form as well
    alga algae
    amoeba amoebas
    antenna antennas
    formula formulas
    larva larvae
    nebula nebulas
    vertebra vertebrae

    ;; Nouns ending in -us with plural -a (only in technical use)
    corpus corpora
    genus genera

    ;; Nouns ending in -us with plural -i
    alumnus alumni
    bacillus bacilli
    cactus cactuses
    focus foci
    fungus fungi
    nucleus nuclei
    octopus octopuses
    radius radii
    stimulus stimuli
    syllabus syllabuses
    terminus termini

    ;; Nouns ending in -um with plural -a
    addendum addenda
    bacterium bacteria
    curriculum curricula
    datum data
    erratum errata
    medium media
    memorandum memorandums
    ovum ova
    stratum strata
    symposium symposiums

    ;; Nouns ending in -ex, -ix becoming plural -ices
    apex apexes
    appendix appendices
    cervix cervices
    index indexes
    matrix matrices
    vortex vortices

    ;; Nouns ending in -is becoming -es in plural
    analysis analyses
    axis axes
    basis bases
    crisis crises
    diagnosis diagnoses
    emphasis emphases
    hypothesis hypotheses
    neurosis neuroses
    oasis oases
    parenthesis parentheses
    synopsis synopses
    thesis theses

    ;; Nouns ending in -on becoming -a
    criterion criteria
    phenomenon phenomena
    automaton automata

    ;; Other irregular plurals, retained from different languages
    libretto libretti ;; Italian
    tempo tempi
    virtuoso virtuosi

    cherub cherubim   ;; Hebrew
    seraph seraphim

    schema schemata   ;; Greek
    })


(def java-primitive-singular
  '{bytes byte
    chars char
    ints int
    longs long
    booleans boolean
    floats float
    doubles double})


(def java-primitive-plural
  (seq-to-map java-primitive-singular identity (keys java-primitive-singular)))


;; CONFIGURE IRREGULAR NOUNS ON COMPILE-TIME

(doseq [[singular plural] IRREGULAR-NOUNS]
  (let [singular (.toLowerCase (str singular))
        plural   (.toLowerCase (str plural))]
    (when-not (= (pluralize singular) plural)
      (org.jactiveresource.Inflector/irregular singular plural))))
