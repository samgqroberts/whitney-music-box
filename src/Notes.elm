module Notes exposing (Note(..), Octave(..), freq)


type alias Frequency =
    Float


type Note
    = Aflt
    | A
    | Ashp
    | Bflt
    | B
    | Bshp
    | Cflt
    | C
    | Cshp
    | Dflt
    | D
    | Dshp
    | Eflt
    | E
    | Eshp
    | Fflt
    | F
    | Fshp
    | Gflt
    | G
    | Gshp


type UniqueNote
    = UA
    | UAshp
    | UB
    | UC
    | UCshp
    | UD
    | UDshp
    | UE
    | UF
    | UFshp
    | UG
    | UGshp


type Octave
    = Oct0
    | Oct1
    | Oct2
    | Oct3
    | Oct4
    | Oct5
    | Oct6
    | Oct7
    | Oct8


toUnique : Note -> UniqueNote
toUnique note =
    case note of
        Aflt ->
            UGshp

        A ->
            UA

        Ashp ->
            UAshp

        Bflt ->
            UAshp

        B ->
            UB

        Bshp ->
            UC

        Cflt ->
            UB

        C ->
            UC

        Cshp ->
            UCshp

        Dflt ->
            UCshp

        D ->
            UD

        Dshp ->
            UDshp

        Eflt ->
            UDshp

        E ->
            UE

        Eshp ->
            UF

        Fflt ->
            UE

        F ->
            UF

        Fshp ->
            UFshp

        Gflt ->
            UFshp

        G ->
            UG

        Gshp ->
            UGshp


freq : Note -> Octave -> Frequency
freq note oct =
    uniqueFreq (toUnique note) oct


uniqueFreq : UniqueNote -> Octave -> Frequency
uniqueFreq note oct =
    case oct of
        Oct0 ->
            case note of
                UC ->
                    16.3516

                UCshp ->
                    17.32391

                UD ->
                    18.35405

                UDshp ->
                    19.44544

                UE ->
                    20.60172

                UF ->
                    21.82676

                UFshp ->
                    23.12465

                UG ->
                    24.49971

                UGshp ->
                    25.95654

                UA ->
                    27.5

                UAshp ->
                    29.13523509488062

                UB ->
                    30.867706328507754

        Oct1 ->
            case note of
                UC ->
                    32.70319566257483

                UCshp ->
                    34.64782887210901

                UD ->
                    36.70809598967595

                UDshp ->
                    38.890872965260115

                UE ->
                    41.20344461410874

                UF ->
                    43.653528929125486

                UFshp ->
                    46.2493028389543

                UG ->
                    48.99942949771866

                UGshp ->
                    51.91308719749314

                UA ->
                    55.0

                UAshp ->
                    58.27047018976124

                UB ->
                    61.735412657015516

        Oct2 ->
            case note of
                UC ->
                    65.40639132514966

                UCshp ->
                    69.29565774421802

                UD ->
                    73.4161919793519

                UDshp ->
                    77.78174593052023

                UE ->
                    82.40688922821748

                UF ->
                    87.30705785825097

                UFshp ->
                    92.4986056779086

                UG ->
                    97.99885899543732

                UGshp ->
                    103.82617439498628

                UA ->
                    110.0

                UAshp ->
                    116.54094037952248

                UB ->
                    123.47082531403103

        Oct3 ->
            case note of
                UC ->
                    130.8127826502993

                UCshp ->
                    138.59131548843604

                UD ->
                    146.8323839587038

                UDshp ->
                    155.56349186104046

                UE ->
                    164.81377845643496

                UF ->
                    174.61411571650194

                UFshp ->
                    184.9972113558172

                UG ->
                    195.99771799087463

                UGshp ->
                    207.65234878997256

                UA ->
                    220.0

                UAshp ->
                    233.08188075904496

                UB ->
                    246.94165062806206

        Oct4 ->
            case note of
                UC ->
                    261.6255653005986

                UCshp ->
                    277.1826309768721

                UD ->
                    293.6647679174076

                UDshp ->
                    311.1269837220809

                UE ->
                    329.6275569128699

                UF ->
                    349.2282314330039

                UFshp ->
                    369.9944227116344

                UG ->
                    391.99543598174927

                UGshp ->
                    415.3046975799451

                UA ->
                    440.0

                UAshp ->
                    466.1637615180899

                UB ->
                    493.8833012561241

        Oct5 ->
            case note of
                UC ->
                    523.2511306011972

                UCshp ->
                    554.3652619537442

                UD ->
                    587.3295358348151

                UDshp ->
                    622.2539674441618

                UE ->
                    659.2551138257398

                UF ->
                    698.4564628660078

                UFshp ->
                    739.9888454232688

                UG ->
                    783.9908719634985

                UGshp ->
                    830.6093951598903

                UA ->
                    880.0

                UAshp ->
                    932.3275230361799

                UB ->
                    987.7666025122483

        Oct6 ->
            case note of
                UC ->
                    1046.5022612023945

                UCshp ->
                    1108.7305239074883

                UD ->
                    1174.6590716696303

                UDshp ->
                    1244.5079348883237

                UE ->
                    1318.5102276514797

                UF ->
                    1396.9129257320155

                UFshp ->
                    1479.9776908465376

                UG ->
                    1567.981743926997

                UGshp ->
                    1661.2187903197805

                UA ->
                    1760.0

                UAshp ->
                    1864.6550460723597

                UB ->
                    1975.5332050244965

        Oct7 ->
            case note of
                UC ->
                    2093.004522404789

                UCshp ->
                    2217.4610478149766

                UD ->
                    2349.3181433392606

                UDshp ->
                    2489.0158697766474

                UE ->
                    2637.0204553029594

                UF ->
                    2793.825851464031

                UFshp ->
                    2959.955381693075

                UG ->
                    3135.963487853994

                UGshp ->
                    3322.437580639561

                UA ->
                    3520.0

                UAshp ->
                    3729.3100921447194

                UB ->
                    3951.066410048993

        Oct8 ->
            case note of
                UC ->
                    4186.009044809578

                UCshp ->
                    4434.922

                UD ->
                    4698.636

                UDshp ->
                    4978.032

                UE ->
                    5274.041

                UF ->
                    5587.652

                UFshp ->
                    5919.911

                UG ->
                    6271.927

                UGshp ->
                    6644.875

                UA ->
                    7040.0

                UAshp ->
                    7458.62

                UB ->
                    7902.133
