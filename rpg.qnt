module rpg {
    type Option[a] =
    | Some(a)
    | None

    type TipoCriatura = Sacerdote | Bardo | Necromante | Inimigo

    type TipoEfeito = Cura | Canto | Lanca

    type Efeito = {
        tipo: TipoEfeito,
        hp: int
    }

    type Criatura = {
        hp: int,
        classe: TipoCriatura,
        paralisia: bool,
        efeito: Option[Efeito],
        iniciativa: int
    }
}