class Foo extends Bar
    abstract native dependson(Baz);

#exec OBJ LOAD FILE=..\\Textures\\DH_InterfaceArt2_tex.utx

const INTEGER = 42;
const FLOAT = 42.0; // TODO: it may be nice to have this always output float style even if it's a round number
const STRING = "String";
const NAME = 'name_literal';
const BOOLEAN_FALSE = false;
const BOOLEAN_TRUE = True;
const VECT = vect(0, 1, 2);
const ROTATOR = rot(0, 1, 2);
const OBJECT = Foo'Bar.Baz';

var localized int Nice[3];
var(Some) int Foo;

enum EAlliedNation
{
    NATION_USA,
    NATION_Britain,
    NATION_Canada,
    NATION_USSR,
    NATION_Poland
};

var array<class<DHConstruction> > A;

struct ArtilleryType
{
    var() int                   TeamIndex;
    var() class<DHArtillery>    ArtilleryClass;             // The artillery class to use.
    var() bool                  bIsInitiallyActive;         // Whether or not this type of artillery is initially available (can be made available and unavailable during a round).
    var() int                   DelaySeconds;               // The amount of seconds it will take until the artillery actor is spawned.
    var() int                   Limit;                      // The amount of these types of artillery strikes that are available.
    var() int                   ConfirmIntervalSeconds;     // The amount of seconds it will take until another request can be confirmed.
};
simulated function bool IsConstructionRestricted(coerce optional out skip array<class<DHConstruction>> ConstructionClass[3])
{
    local int i;

    for (i = 0; i < RestrictedConstructions.Length; ++i)
    {
        if (ConstructionClass == RestrictedConstructions[i])
        {
            return true;
        }
    }

    return false;
}